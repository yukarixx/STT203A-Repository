library(shiny)
library(readxl)

ui <- fluidPage(
  titlePanel("Sleep Health Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Dataset (.csv or .xlsx)",
                accept = c(".csv", ".xlsx")),
      verbatimTextOutput("status")
    ),
    
    mainPanel(
      h4("Data Preview"),
      tableOutput("preview")
    )
  )
)

server <- function(input, output) {
  
  # Reactive value to store data
  user_data <- reactiveVal(NULL)
  
  # Handle file upload
  observeEvent(input$file, {
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    df <- NULL
    
    # Read file based on extension
    if(ext == "csv") {
      df <- read.csv(input$file$datapath)
    } else if(ext == "xlsx") {
      df <- read_excel(input$file$datapath)
    }
    
    # Validate: Check rows
    if(nrow(df) < 250) {
      output$status <- renderPrint({
        cat("ERROR: File has", nrow(df), "rows. Need at least 250 rows.")
      })
      return()
    }
    
    # Validate: Check columns
    if(ncol(df) < 5) {
      output$status <- renderPrint({
        cat("ERROR: File has", ncol(df), "columns. Need at least 5 columns.")
      })
      return()
    }
    
    # If validation passes
    user_data(df)
    
    output$status <- renderPrint({
      cat("SUCCESS!", nrow(df), "rows,", ncol(df), "columns")
    })
  })
  
  # Show preview of data
  output$preview <- renderTable({
    req(user_data())
    head(user_data(), 10)
  })
}

shinyApp(ui, server)

testing