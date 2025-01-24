library(shiny)
library(shinyWidgets)
library(DT)
library(shiny)
library(shinyjs)

# Define UI for application 

ui <- fluidPage(
  
  tags$div(
    style = "align-items: left; justify-content: space-between; margin-bottom: 20px;",
    # Logo
    tags$img(src = "LogosCurie800px.png", alt = "Institut Curie Logo", height = "100px", style = "margin-right: 20px;"),
    # Unit name and link
    tags$div(
      style = "text-align: left;",
      tags$h4("U1331 - Computational Oncology", style = "margin: 0;"),
      tags$a(href = "https://curie.fr/unite/u1331", "https://curie.fr/unite/u1331", target = "_blank", style = "font-size: 16px;")
    )
  ),
  
  
  #setBackgroundColor("ghostwhite"),
  
  titlePanel("Deconvolution toolbox for spatial transcriptomics"),
  
  p("This table serves as a supplementary resource to the review ",strong('"The expanding toolbox of cell-type deconvolution for spatial transcriptomics"'),em("(Gaspard-Boulinc, L., Gortana, L. et al., Nature Reviews Genetics, in revision)"), 
  " providing detailed information on the specific methods discussed. 
    It allows you to navigate through various methods, exploring their distinct characteristics and features. 
    Use this interactive table to delve deeper into the nuances of each method and enhance your understanding of the review's content. You can ",strong("filter based on 6 criteria"), "and display the 31 items reported for each methods. It is also possible to ",strong("click on a row"), "to display all the method's information below the table.",
    class = "intro-paragraph", style = "font-size: 18px;"
  ),
  
  p(
    "If you would like to add your recently developed methods, you can fill this form ", 
    tags$a(href = "https://forms.gle/fuUDhgMrfYvY4uQXA", "here", target = "_blank"), 
    ". If you have any question regarding the details, feel free to contact us at ", 
    tags$a(href = "mailto:lucie.gaspard-boulinc@curie.fr", "lucie.gaspard-boulinc[at]curie.fr", target = "_blank"), 
    " and ", 
    tags$a(href = "mailto:luca.gortana@curie.fr", "luca.gortana[at]curie.fr", target = "_blank"), ".",
    class = "intro-paragraph", style = "font-size: 18px;"
  ),
  
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 
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
                                           "Probabilities", "Cell location", "Super-pixel gene expression"),  # 
                               selected = NULL),
                   
                   # User input to filter based on Programming languages
                   selectInput("ProgrValue", 
                               "Programming language:",
                               choices = c("All","R","Python","MATLAB"),  # Unique items
                               selected = NULL)
                 ),
                 
                 
                 div(
                   style = "margin-top: 20px; padding: 10px; border-top: 2px solid #007bff; border-radius: 5px;",
                   h4("Column Selection"),
                   # Input to select columns to display
                   uiOutput("columnSelection")
                 )
                 
                 
    ),
    mainPanel(width = 10,
              selectInput("n_rows", "Show ... entries", choices =  c("5"=5, "15"=15, "30"=30, "45"=45, "All"=-1)),
              
              
              dataTableOutput("myTable"),
              uiOutput("detailedOutput") 
    )
  )
)