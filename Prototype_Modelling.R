library(shinyalert)
library(shiny)
library(randomForest)

setwd("C:/Users/tyeck/OneDrive/Desktop/School/Y2S1/BC2406 Analytics I Visual & Predictive Techniques/BC2406_Project_Submission_Group4")


# Load the model when the app starts
model <- readRDS("random_forest_model.rds")

# Load the datset for reference
emissionsData.dt <- data.table::fread("cleaned_methane_emissions_data.csv",stringsAsFactors = TRUE, na.strings = c("NA", "missing", "N/A", -99, "", "m", "M", "na", "."))



# Define the user interface
ui <- fluidPage(
  useShinyalert(),
  titlePanel("Random Forest Prediction for Methane Emissions"),
  sidebarLayout(
    sidebarPanel(
      # Create input fields based on the features of your model
      numericInput("wellCount", "Number of Wells", value = 5),
      numericInput("measuredEquipmentLeaks", "Measured Equipment Leaks", value = 2),
      numericInput("measuredChemicalInjectionPumps", "Measured Chemical Injection Pumps", value = 5),
      numericInput("measuredTanks", "Measured Tanks", value = 5),
      numericInput("hcProduced", "Hydrocarbons Produced", value = 2771.84),
      numericInput("waterProduced", "Water Produced", value = 524),
      numericInput("gasProduced", "Gas Produced", value = 7.34),
      numericInput("wellheadPressure", "Wellhead Pressure", value = 1879),
      numericInput("age", "Age of the Well", value = 5.91),
      numericInput("totalPneumaticControllers", "Total Pneumatic Controllers", value = 6),
      numericInput("totalChemicalInjectionPumps", "Total Chemical Injection Pumps", value = 7),
      selectInput("producingType", "Producing Type", choices = c("shale gas", "Other Tight Reservoir Rock", "Other Tight Reservoir Rock & Shale")),
      selectInput("wellStruct", "Well Structure", choices = c("Horizontal", "Directional", "Vertical")),
      selectInput("combuster", "Combuster Installed", choices = c("No", "Yes")),
      selectInput("tracerflux", "Tracer Flux Used", choices = c("No", "Yes")),
      actionButton("predict", "Predict"),
      
      tags$head(
        tags$style(HTML("
    body {
      background-image: url('https://images.news18.com/ibnlive/uploads/2022/05/saudi-aramco-16523366654x3.jpg');
      background-size: cover;
    }
  "))
      )
    ),
    mainPanel(
      textOutput("result")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Use reactiveValues to store the prediction and category
  prediction_results <- reactiveValues(value = NULL, category = NULL)
  
  # Function to run prediction
  observeEvent(input$predict, {
    new_data <- data.frame(
      WellCount = as.integer(input$wellCount),
      MeasuredEquipmentLeaks = as.integer(input$measuredEquipmentLeaks),
      MeasuredChemicalInjectionPumps = as.numeric(input$measuredChemicalInjectionPumps),
      MeasuredTanks = as.numeric(input$measuredTanks),
      HCProduced = as.numeric(input$hcProduced),
      WaterProduced = as.numeric(input$waterProduced),
      GasProduced = as.numeric(input$gasProduced),
      Wellheadpressure = as.numeric(input$wellheadPressure),
      Age = as.numeric(input$age),
      TotalPneumaticControllers = as.numeric(input$totalPneumaticControllers),
      TotalChemicalInjectionPumps = as.integer(input$totalChemicalInjectionPumps),
      ProducingType = as.factor(input$producingType),
      WellStruct = as.factor(input$wellStruct),
      Combuster = as.factor(input$combuster),
      tracerflux = as.factor(input$tracerflux),
      stringsAsFactors = FALSE
    )
    # Function to categorize the prediction
    categorize_prediction <- function(value) {
      if (value < 85.5) {
        return("Acceptable")
      } else if (value >= 85.5 & value <= 90) {
        return("Moderately Acceptable")
      } else {
        return("Unacceptable")
      }
    }
    # Convert factors to the correct levels based on your original training data
    new_data$ProducingType <- factor(new_data$ProducingType, levels = levels(emissionsData.dt$ProducingType))
    new_data$WellStruct <- factor(new_data$WellStruct, levels = levels(emissionsData.dt$WellStruct))
    new_data$Combuster <- factor(new_data$Combuster, levels = levels(emissionsData.dt$Combuster))
    new_data$tracerflux <- factor(new_data$tracerflux, levels = levels(emissionsData.dt$tracerflux))
    
    # Predict using the Random Forest model
    predicted_value<- predict(model, new_data)
    
    # Categorize the predicted value
    predicted_category <- categorize_prediction(predicted_value)
    
    # Return both the predicted value and the category
    list(value = predicted_value, category = predicted_category)
    
    shinyalert::shinyalert(title = "Prediction Result", 
                           text = paste("Predicted Emissions:", round(predicted_value, 3), " scfm",
                                        "\nCategory:", predicted_category),
                           type = "info")
    
  })
  
  output$result <- renderText({
    req(prediction_results$value) # Require that a prediction has been made
    paste("Predicted emissions sum:", round(prediction_results$value, 3), "scfm", 
          "\nCategory:", prediction_results$category)
  })
}


# Run the application
shinyApp(ui = ui, server = server)

