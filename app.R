library(shiny)
library(dplyr)
library(DT)
library(openxlsx)
library(htmltools)
library(tidyverse)

# Load the dataset
data <- read.xlsx("example.xlsx")

# Define UI for the application
ui <- navbarPage(
  title = "Sliders",
  
  # Tab for selecting columns
  tabPanel("Column Selection",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("columns", "Select Columns:",
                                  choices = names(data)[sapply(data, is.numeric)],
                                  selected = names(data)[sapply(data, is.numeric)]),
               actionButton("deselect_all", "Deselect All")
             ),
             mainPanel(
               p("Select which columns to include in the data table, sliders, and export.")
             )
           )
  ),
  
  # Tab for displaying the selected columns from the original data
  tabPanel("Selected Columns",
           mainPanel(
             DTOutput("selected_table")
           )
  ),
  
  # Tab for displaying and exporting normalized data
  tabPanel("Normalized Data Table",
           sidebarLayout(
             sidebarPanel(
               uiOutput("sliderInputs"),  # Dynamically generated sliders
               downloadButton("downloadData", "Download Data")
             ),
             mainPanel(
               DTOutput("table")
             )
           )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Observe the "Deselect All" button click
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "columns", selected = character(0))
  })
  
  # Dynamically generate slider and numeric inputs based on selected columns
  output$sliderInputs <- renderUI({
    selected_cols <- input$columns
    inputs <- lapply(selected_cols, function(col) {
      sliderInputId <- paste0("mult_", col)
      numericInputId <- paste0("num_", col)
      
      fluidRow(
        column(6, sliderInput(inputId = sliderInputId, 
                              label = paste("Multiplier", col), 
                              min = 1, max = 10, value = 1)),
        column(4, numericInput(inputId = numericInputId, 
                               label = "Input", 
                               value = 1))
      )
    })
    do.call(tagList, inputs)
  })
  
  # Synchronize the slider and numeric inputs
  observe({
    selected_cols <- input$columns
    lapply(selected_cols, function(col) {
      sliderInputId <- paste0("mult_", col)
      numericInputId <- paste0("num_", col)
      
      observeEvent(input[[sliderInputId]], {
        updateNumericInput(session, numericInputId, value = input[[sliderInputId]])
      })
      
      observeEvent(input[[numericInputId]], {
        updateSliderInput(session, sliderInputId, value = input[[numericInputId]])
      })
    })
  })
  
  # Reactive data frame that normalizes selected columns and applies multipliers
  reactive_data <- reactive({
    selected_cols <- input$columns
    
    # Exclude the "Postcode" column from normalization and multiplication
    cols_to_normalize <- setdiff(selected_cols, "Postcode")
    
    df <- data %>% select(Postcode, all_of(cols_to_normalize))
    
    # Normalize the selected columns (excluding Postcode)
    df <- df %>%
      mutate(across(all_of(cols_to_normalize), ~ scale(.) %>% as.numeric()))
    
    # Apply the multiplier to each normalized column
    for (col in cols_to_normalize) {
      multiplier <- input[[paste0("mult_", col)]]
      if (!is.null(multiplier)) {
        df[[col]] <- round(df[[col]] * multiplier, 2)
      }
    }
    
    # Add a Score column as the product of all normalized and multiplied columns
    df$Score <- apply(df[, cols_to_normalize], 1, prod)
    
    # Move the Score column to the first position
    df <- df %>% select(Score, Postcode, everything())
    df
  })
  
  # Reactive data frame for selected columns from original data
  reactive_selected_data <- reactive({
    data %>% select(all_of(input$columns))
  })
  
  # Render the data table for selected columns
  output$selected_table <- renderDT({
    datatable(reactive_selected_data(), options = list(pageLength = 10))
  })
  
  # Render the normalized data table with applied multipliers
  output$table <- renderDT({
    normalized_data <- reactive_data()
    datatable(normalized_data, options = list(pageLength = 10))
  })
  
  # Download handler for the normalized data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("edit-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      normalized_data <- reactive_data()
      write.xlsx(normalized_data, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
