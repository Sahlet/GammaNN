library(shiny)
library(dygraphs)
#shinyUI(

fluidPage(
  titlePanel("Random processes Lab1"),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Learning",
          br(),
          fileInput('CSV_loader', 'Choose CSV File',
                    accept=c('text/csv',
                             'text/comma-separated-values,text/plain',
                             '.csv')),
          #        tags$hr(),
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator', c( Space = '', Comma=',', Semicolon=';' ), ','),
          br(),
          br(),
          sliderInput("training_data_percent", "training data percent",
                      min = 30, max = 80, value = 60, step= 1),
          numericInput("hidden_layers", "Hidden layers number", 3, min = 0, max = 20, step = 1),
          numericInput("hidden_layers_width", "Hidden layers width", 3, min = 1, max = 10, step = 1),
          numericInput("gamma_units", "Gamma units number", 5, min = 0, max = 20, step = 1),
          numericInput("trace_size", "Trace size", 1, min = 1, max = 20, step = 1),
          br(),
          br(),
          conditionalPanel(
            condition = "output.learn_button_is_visible",
            actionButton("learn", "Learn")
          )
        ),
        tabPanel(
          "Model Serialization",
          br(),
          fileInput('NN_uploader', 'Upload NN File'),
          conditionalPanel (
            condition = "output.NN_exists",
            downloadButton('NN_downloader', 'Download GammaNN File')
          )
        ),
        tabPanel(
          "Prediction",
          br(),
          conditionalPanel (
            condition = "output.NN_exists",
            sliderInput("prediction_range",
                        label = "Range:",
                        min = 1, max = 500,
                        value = c(1, 10),
                        step = 1
            )
          )
        )
      )
    ),
    mainPanel(
      #verbatimTextOutput("test"),
      tags$div(id = 'placeholder')
    )
  )

)

# fluidPage(
#   actionButton('insertBtn1', 'Insert placeholder2'),
#   actionButton('insertBtn', 'Insert'),
#   actionButton('removeBtn', 'Remove'),
#   
#   titlePanel("Predicted Deaths from Lung Disease (UK)"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       numericInput("months", label = "Months to Predict",
#                    value = 72, min = 12, max = 144, step = 12),
#       selectInput("interval", label = "Prediction Interval",
#                   choices = c("0.80", "0.90", "0.95", "0.99"),
#                   selected = "0.95"),
#       checkboxInput("showgrid", label = "Show Grid", value = TRUE)
#     ),
#     mainPanel(
#       tags$div(id = 'placeholder'),
#       dygraphOutput("dygraph")
#     )
#   )
# )

#)