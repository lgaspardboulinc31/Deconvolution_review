library(shiny)
library(shinyWidgets)
library(DT)
library(shiny)
library(shinyjs)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  file_path <- "./data/deconvolution_methods_v4.csv" #"./data/deconvolution_table_rebuttal_v3f.csv"
  
  
  # Load data
  
  data <- reactive({
    df <- read.csv(file_path, sep = ";", fileEncoding = "UTF-8")
    
    # Format columns with clickable links
    ## For code
    columns_to_format <- c("URL", "DOI", "Code", "Tutorials")
    for (col in columns_to_format) {
      if (col %in% colnames(df)) {
        df[[col]] <- ifelse(
          !is.na(df[[col]]) & df[[col]] != "",
          paste0('<a href="', df[[col]], '" target="_blank">', df[[col]], '</a>'),
          df[[col]]
        )
      }
    }
    df <- df[order(df$Method.name),]
    df
  })
  
  # Populate filter choices
  observe({
    df <- data()
    updateSelectInput(session, "RefValue", 
                      choices = c("All", unique(df$Reference.based_Reference.free)))
    updateSelectInput(session, "CategoryValue", 
                      choices = c("All", unique(as.character(df$Category))))
    updateSelectInput(session, "CoordValue", 
                      choices = c("All", unique(df$ST.coordinates)))
    updateSelectInput(session, "ImgValue", 
                      choices = c("All", unique(df$Image)))
  })
  
  # Render column selection UI
  output$columnSelection <- renderUI({
    df <- data()
    checkboxGroupInput("columns", "Select columns to display:",
                       choices = c("All",colnames(df)),
                       selected = c("Method.name", "Reference.based_Reference.free", "ST.coordinates", "Image",
                                    "Programming.language", "Code"))
  })
  
  # Reactive for filtering data based on user inputs
  filteredData <- reactive({
    df <- data()
    if (input$RefValue != "All") {
      df <- df[df$Reference.based_Reference.free %in% c(input$RefValue, "Both"), ]
    }
    if (input$CategoryValue != "All") {
      df <- df[df$Category == input$CategoryValue, ]
    }
    if (input$CoordValue != "All") {
      df <- df[df$ST.coordinates %in% c(input$CoordValue, "Optional"), ]
    }
    if (input$ImgValue != "All") {
      df <- df[df$Image %in% c(input$ImgValue, "Optional"), ]
    }
    if (input$OutValue != "All") {
      selected_option <- input$OutValue
      df <- df[grepl(selected_option, df$Main.output), ]
    }
    if (input$ProgrValue != "All") {
      selected_option <- input$ProgrValue
      df <- df[grepl(selected_option, df$Programming.language), ]
    }
    df
  })
  
  # Reactive for displaying only selected columns
  displayedData <- reactive({
    df <- filteredData()
    
    selected_columns <- input$columns
    
    if (is.null(selected_columns)) {
      df  # If no columns are selected, show all
    } else if ("All" %in% selected_columns) {
      df  # If "All" is selected, show all columns
    } else {
      df[, selected_columns, drop = FALSE]  # Show only the selected columns
    }
  })
  
  
  
  # Render the table with dynamic column selection
  output$myTable <- renderDataTable({
    nrow <- as.numeric(input$n_rows)
    datatable(
      displayedData(), 
      options = list(autoWidth = TRUE, lengthChange=F,
                     columnDefs = list(
                       list(targets = "_all", width = "auto")),
                     # stateSave = TRUE,
                     pageLength =  nrow),
      selection = "single",
      escape = FALSE ,
      rownames= FALSE)
  })
  
  # Reactive for selected row
  selected_row <- reactive({
    selected <- input$myTable_rows_selected
    if (is.null(selected)) return(NULL)
    filteredData()[selected, , drop = FALSE]
  })
  
  # Display selected row details in vertical format
  output$detailedOutput <- renderUI({
    row <- selected_row()
    if (is.null(row)) return(NULL)
    
    # Get the method name for the title
    method_name <- if ("Method.name" %in% names(row)) row[["Method.name"]] else "Details"
    
    
    vertical_layout <- lapply(names(row), function(col_name) {
      # Get the cell value for the current column
      cell_value <- row[[col_name]]
      
      # Check if the column has HTML links (i.e., if it is already an <a> tag)
      if (col_name %in% c("URL", "DOI", "Code", "Tutorials")) {
        # Directly use the HTML content from the cell without modification
        return(tagList(
          tags$b(col_name), ": ", HTML(cell_value), tags$br()
        ))
      } else {
        # For other columns, just display the value as plain text
        return(tagList(
          tags$b(col_name), ": ", cell_value, tags$br()
        ))
      }
    })
    
    wellPanel(
      h4(paste0(method_name, " details:")),
      do.call(tagList, vertical_layout)
    )
  })
}
