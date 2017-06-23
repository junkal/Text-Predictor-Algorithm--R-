library(shiny)

shinyUI(fluidPage(
  
  # Application title
  headerPanel("Text Predictor"),
  
  mainPanel(
    helpText("Type words into the input box and press 'Predict' to see the predicted next word"),
    hr(),
    textInput("word", "Input Box"),
    br(),
    actionButton("predict", label = "Predict"),
    hr(),
    
    h5("1st Prediction: "),
    fluidRow(column(5, verbatimTextOutput("text1"))),
        
    h5("2nd Prediction: "),
    fluidRow(column(5, verbatimTextOutput("text2"))),
    
    h5("3rd Prediction: "),
    fluidRow(column(5, verbatimTextOutput("text3")))
  )
))