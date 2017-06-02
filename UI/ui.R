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
          conditionalPanel(
            condition = "output.learn_button_is_visible",
            sliderInput("training_data", "Training data",
                        value = 60, min = 30, max = 80, step= 1),
            sliderInput("hidden_layers", "Hidden layers number", value = 2, min = 0, max = 20, step = 1),
            sliderInput("hidden_layers_width", "Hidden layers width", value = 3, min = 1, max = 20, step = 1),
            sliderInput("gamma_units", "Gamma units number", value = 2, min = 0, max = 20, step = 1),
            sliderInput("trace_size", "Trace size", value = 1, min = 1, max = 20, step = 1),
            numericInput("eps", "Eps", value = 0.01, min = 0.001, step = 0.001),
            sliderInput("batch_size", "Batch size", value = 1, min = 1, max = 1, step = 1),
            checkboxInput('random_patterns', 'Random patterns', FALSE),
            sliderInput("max_epoch_number", "Max epoch", value = 5000, min = 1000, max = 100000, step = 1000),
            br(),
            br(),
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

#)