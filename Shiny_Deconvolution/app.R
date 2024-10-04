#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  setBackgroundColor("ghostwhite"),

  titlePanel("Deconvolution toolbox for spatial transcriptomics"),
  
  p("This table serves as a supplementary resource to the review, providing detailed information on the specific methods discussed. 
    It allows you to navigate through various methods, exploring their distinct characteristics and features. 
    Use this interactive table to delve deeper into the nuances of each method and enhance your understanding of the review's content.",
    class = "intro-paragraph"
  ),
  sidebarLayout(
    sidebarPanel(
      
      div(
        style = "margin-top: 20px; padding: 10px; border-top: 2px solid #007bff; border-radius: 5px;",
        h4("Filter methods"),
      # User input to filter based on ref-free/ref-based
      selectInput("RefValue", 
                  "Use annotated scRNA-seq:",
                  choices = NULL,  # Choices will be populated in the server
                  selected = NULL),
      
      # User input to filter based on algorithm class
      selectInput("CategoryValue", 
                  "Type of algorithm:",
                  choices = NULL,  # Choices will be populated in the server
                  selected = NULL),
      
      # User input to filter based on coordinates use
      selectInput("CoordValue", 
                  "Use coordinates:",
                  choices = NULL,  # Choices will be populated in the server
                  selected = NULL),
      
      # User input to filter based on image use
      selectInput("ImgValue", 
                  "Use image:",
                  choices = NULL,  # Choices will be populated in the server
                  selected = NULL),
      
      # User input to filter based on output type
      selectInput("OutValue", 
                  "Output type:",
                  choices = c("All","Proportions","Counts","Mapping", "Single-cell gene expression",
                              "Probabilities", "Cell location", "Super-pixel gene expression"),  # Choices will be populated in the server
                  selected = NULL)
      
      ),
      
      div(
        style = "margin-top: 20px; padding: 10px; border-top: 2px solid #007bff; border-radius: 5px;",
        h4("Column Selection"),
      # Input to select columns to display
      uiOutput("columnSelection")
      )
      
      
    ),
    mainPanel(
      dataTableOutput("myTable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # deconvolution table
  file_path <- "./data/deconvolution_method_table_vf.csv"
  
  # Read the CSV file into a data frame
  data <- reactive({
    read.csv(file_path, sep = ";" , fileEncoding = "UTF-8") #csv in utf-8 format
  })
  
  
  
  # Populate the filter choices based on unique values in the "Occupation" column
  observe({
    df <- data()
    updateSelectInput(session, "RefValue", 
                      choices = c("All", unique(df$Reference.based...Reference.free)))
    
    updateSelectInput(session, "CategoryValue", 
                      choices = c("All", unique(as.character(df$Category))))
    
    updateSelectInput(session, "CoordValue", 
                      choices = c("All", unique(df$ST.coordinates)))
    
    updateSelectInput(session, "ImgValue", 
                      choices = c("All", unique(df$Image)))
    
    #updateSelectInput(session, "OutValue", 
                      #choices = c("All", unique(df$Main.output)))
  })
  
  # Tailor which columns to show
  output$columnSelection <- renderUI({
    df <- data()
    checkboxGroupInput("columns", "Select columns to display:",
                       choices = colnames(df),
                       selected = c("Method.name", "Reference.based...Reference.free", "ST.coordinates", "Image", "Code"))
  })
  
  
  # Filter the data based on user inputs
  filteredData <- reactive({
    df <- data()
    if (input$RefValue != "All") {
      df <- df[df$Reference.based...Reference.free %in% c(input$RefValue,"Both"), ]#count with Both
    }
    if (input$CategoryValue != "All") {
      df <- df[df$Category == input$CategoryValue, ]
    }
    if (input$CoordValue != "All") {
      df <- df[df$ST.coordinates %in%  c(input$CoordValue,"Optional"), ]#count with optional
    }
    if (input$ImgValue != "All") {
      df <- df[df$Image %in% c(input$ImgValue, "Optional"), ] #count with optional
    }
    if (input$OutValue != "All") {
      selected_option <- input$OutValue
      df <- df[grepl(selected_option, df$Main.output),  ]
    }
    df
    selected_columns <- input$columns
    df[, selected_columns, drop = FALSE]
  })
  
  
  
  # Output the interactive table to the UI
  output$myTable <- renderDataTable({
    datatable(filteredData())
  })
}

    

# Run the application 
shinyApp(ui = ui, server = server)

##
#library(rsconnect)
# rsconnect::deployApp("/Users/lgaspard/Documents/Literature/Deconvolution_review/Shiny_Deconvolution")
