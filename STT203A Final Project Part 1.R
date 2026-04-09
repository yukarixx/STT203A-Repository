library(shiny)
library(readxl)
#PHASE 1 1st bullet
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
#PHASE 1 2ND BULLET
library(dplyr)
tabPanel("Visualization",
  sidebarLayout(
    sidebarPanel(
      h4("Map Variables to Axes"),
      
      selectInput("x_var", "X-axis Variable", 
                 choices = c("(Select variable)" = "")),
      
      selectInput("y_var", "Y-axis Variable (Numeric)", 
                 choices = c("(None)" = "")),
      
      selectInput("color_var", "Color/Group Variable (Categorical)", 
                 choices = c("(None)" = "")),
      
      selectInput("plot_type", "Plot Type",
                 choices = c(
                   "Scatter Plot" = "scatter",
                   "Bar Plot" = "bar",
                   "Box Plot" = "box",
                   "Histogram" = "histogram"
                 )),
      
      hr(),
      helpText("Note: Scatter/Box plots need numeric Y-axis")
    ),
    
    mainPanel(
      h4("Variable Types Detected"),
      verbatimTextOutput("var_types_display"),
      br(),
      h4("Interactive Plot"),
      plotOutput("dynamic_plot", height = "400px")
    )
  )
)
df <- df %>%
  mutate(across(where(is.character), as.factor))

# Update dropdown choices
updateSelectInput(session, "x_var", choices = c("(Select variable)" = "", names(df)))
updateSelectInput(session, "y_var", choices = c("(None)" = "", names(df)[sapply(df, is.numeric)]))
updateSelectInput(session, "color_var", choices = c("(None)" = "", names(df)[sapply(df, is.factor)]))

# Add this output to your server (after the preview output)

output$var_types_display <- renderPrint({
  req(user_data())
  df <- user_data()
  
  cat("📊 VARIABLE TYPE DETECTION\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━\n\n")
  
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  cat("🔢 NUMERIC variables (", length(numeric_vars), "):\n")
  for(var in numeric_vars) {
    cat("   • ", var, "\n")
  }
  
  cat("\n🏷️ CATEGORICAL variables (", sum(sapply(df, is.factor)), "):\n")
  categorical_vars <- names(df)[sapply(df, is.factor)]
  for(var in categorical_vars) {
    cat("   • ", var, " (", length(levels(df[[var]])), " unique values)\n")
  }
})

# Add this output to your server

output$dynamic_plot <- renderPlot({
  req(user_data(), input$x_var)
  
  df <- user_data()
  x_var <- input$x_var
  y_var <- input$y_var
  color_var <- input$color_var
  plot_type <- input$plot_type
  
  # Create plot based on type
  if(plot_type == "scatter") {
    req(y_var != "")
    if(color_var != "" && color_var != "(None)") {
      ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[color_var]])) +
        geom_point() + theme_minimal()
    } else {
      ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point() + theme_minimal()
    }
    
  } else if(plot_type == "bar") {
    if(color_var != "" && color_var != "(None)") {
      ggplot(df, aes(x = .data[[x_var]], fill = .data[[color_var]])) +
        geom_bar() + theme_minimal()
    } else {
      ggplot(df, aes(x = .data[[x_var]])) +
        geom_bar() + theme_minimal()
    }
    
  } else if(plot_type == "box") {
    req(y_var != "")
    if(color_var != "" && color_var != "(None)") {
      ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[color_var]])) +
        geom_boxplot() + theme_minimal()
    } else {
      ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_boxplot() + theme_minimal()
    }
    
  } else if(plot_type == "histogram") {
    if(color_var != "" && color_var != "(None)") {
      ggplot(df, aes(x = .data[[x_var]], fill = .data[[color_var]])) +
        geom_histogram(bins = 30, alpha = 0.7) + theme_minimal()
    } else {
      ggplot(df, aes(x = .data[[x_var]])) +
        geom_histogram(bins = 30) + theme_minimal()
    }
  }
})