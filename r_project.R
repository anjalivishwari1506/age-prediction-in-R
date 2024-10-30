# Load necessary libraries
library(shiny)

# Load the dataset and train the model
data <- read.csv("C:\\Users\\rimjh\\Downloads\\NHANES_age_prediction.csv")

# Convert categorical variables to factors if needed
data$RIAGENDR <- as.factor(data$RIAGENDR)
data$DIQ010 <- as.factor(data$DIQ010)
data$PAQ605 <- as.factor(data$PAQ605)

# Fit the linear regression model
linear_model <- lm(RIDAGEYR ~ RIAGENDR + PAQ605 + BMXBMI + LBXGLU + DIQ010 + LBXGLT + LBXIN, data = data)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Predict Age using Health Metrics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("RIAGENDR", "Gender:", choices = c("Male" = 1, "Female" = 2)),
      selectInput("PAQ605", "Physical Activity Level:", choices = c("Sedentary" = 1, "Active" = 2)),
      numericInput("BMXBMI", "BMI:", value = 25, min = 10, max = 50, step = 0.1),
      numericInput("LBXGLU", "Glucose Level (mg/dL):", value = 90, min = 50, max = 200),
      selectInput("DIQ010", "Diabetes Status:", choices = c("Yes" = 1, "No" = 2)),
      numericInput("LBXGLT", "Glucose Tolerance Level:", value = 80, min = 50, max = 250),
      numericInput("LBXIN", "Insulin Level (μU/mL):", value = 5, min = 0, max = 100),
      actionButton("predict", "Predict Age")
    ),
    
    mainPanel(
      h2(textOutput("age_prediction")),
      # plotOutput("residualsPlot")
    )
  )
)

# Define server logic for predictions
server <- function(input, output) {
  observeEvent(input$predict, {
    # Create a data frame with the user's input values
    user_input <- data.frame(
      RIAGENDR = as.factor(input$RIAGENDR),
      PAQ605 = as.factor(input$PAQ605),
      BMXBMI = input$BMXBMI,
      LBXGLU = input$LBXGLU,
      DIQ010 = as.factor(input$DIQ010),
      LBXGLT = input$LBXGLT,
      LBXIN = input$LBXIN
    )
    
    # Predict the age based on user input
    predicted_age <- predict(linear_model, newdata = user_input)
    
    # Display the prediction
    output$age_prediction <- renderText({
      paste("Predicted Age: ", round(predicted_age, 2), " years")
    })
  })
  
  # Optional: Plot residuals for diagnostics
  #output$residualsPlot <- renderPlot({
  #plot(linear_model)
  #})
}

# Run the application
shinyApp(ui = ui, server = server)
