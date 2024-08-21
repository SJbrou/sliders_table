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
    # Filter out the "Postcode" column
    cols_to_adjust <- setdiff(selected_cols, "Postcode")
    
    inputs <- lapply(cols_to_adjust, function(col) {
      sliderInputId <- paste0("mult_", col)
      numericInputId <- paste0("num_", col)
      
      fluidRow(
        column(6, sliderInput(inputId = sliderInputId, 
                              label = paste("Weight", col), 
                              min = -4, max = 4, value = 1)),
        column(4, numericInput(inputId = numericInputId, 
                               label = "Input", 
                               value = 1))
      )
    })
    
    # Combine all inputs into a tag list
    do.call(tagList, inputs)
  })
  
  # Synchronize the slider and numeric inputs
  observe({
    selected_cols <- input$columns
    # Filter out the "Postcode" column
    cols_to_adjust <- setdiff(selected_cols, "Postcode")
    
    lapply(cols_to_adjust, function(col) {
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
  
  # Reactive data frame that scales selected columns and applies multipliers
  reactive_data <- reactive({
    selected_cols <- input$columns
    
    # Exclude the "Postcode" column from scaling and multiplication
    cols_to_scale <- setdiff(selected_cols, "Postcode")
    
    df <- data %>% select(Postcode, all_of(cols_to_scale))
    
    # Min-Max scaling (0 to 1) for the selected columns
    df <- df %>%
      mutate(across(all_of(cols_to_scale), 
                    ~ ( . - min(.) ) / ( max(.) - min(.) )))
    
    # Apply the multiplier to each scaled column
    for (col in cols_to_scale) {
      multiplier <- input[[paste0("mult_", col)]]
      if (!is.null(multiplier)) {
        df[[col]] <- round(df[[col]] * multiplier, 2)
      }
    }
    
    # Calculate the weighted average score
    weights <- sapply(cols_to_scale, function(col) input[[paste0("mult_", col)]])
    df$Score <- rowSums(df[, cols_to_scale] * weights, na.rm = TRUE) / sum(weights)
    
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
  
  # Render the scaled data table with applied multipliers
  output$table <- renderDT({
    scaled_data <- reactive_data()
    datatable(scaled_data, options = list(pageLength = 10))
  })
  
  # Download handler for the scaled data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("scaled-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      scaled_data <- reactive_data()
      write.xlsx(scaled_data, file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
