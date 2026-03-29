library(shiny)
library(ggplot2)
library(bslib)
library(readxl)

ui <- page_sidebar(
  title = "Sleep, Health, and Lifestyle Dashboard",
  sidebar = "Sidebar", "Main content area",
  fileInput("file", "Plase Upload CSV or Excel File", accept = c(".csv", ".xlsx")),
  verbatimTextOutput("status")
)

server <- function(input, output) {
  output$status <- renderPrint({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    df <- if(ext == "csv") read.csv(input$file$datapath) else read_excel(input$file$datapath)
    
    if(nrow(df) < 250) {
      cat("Need at least 250 rows. Currently has", nrow(df))
    } else if(ncol(df) < 5) {
      cat("Need at least 5 columns. Currently has", ncol(df))
    } else {
      cat("Valid Dataset!:", nrow(df), "rows,", ncol(df), "columns")
    }
  })
}

shinyApp(ui, server)