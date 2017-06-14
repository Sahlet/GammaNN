library(shiny)
library(dygraphs)
#shinyUI(

fluidPage(
  titlePanel("Time Series prediction using GammaNN"),

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
            checkboxInput('rectangle_hidden', 'Rectangle hidde nodes block', TRUE),
            conditionalPanel(
              condition = "input.rectangle_hidden",
              sliderInput("hidden_layers", "Hidden layers number", value = 2, min = 0, max = 20, step = 1),
              sliderInput("hidden_layers_width", "Hidden layers width", value = 3, min = 1, max = 30, step = 1)
            ),
            conditionalPanel(
              condition = "!input.rectangle_hidden",
              textInput("hidden_layers_text", "Hidden layers")
            ),
            sliderInput("gamma_units", "Gamma units number", value = 2, min = 0, max = 50, step = 1),
            sliderInput("trace_size", "Trace size", value = 1, min = 1, max = 50, step = 1),
            numericInput("eps", "Eps", value = 0.01, min = 0.001, step = 0.001),
            sliderInput("batch_size", "Batch size", value = 1, min = 1, max = 1, step = 1),
            checkboxInput('random_patterns', 'Random patterns', FALSE),
            sliderInput("max_epoch_number", "Max epoch", value = 5000, min = 0, max = 200000, step = 1000),
            sliderInput("max_epoch_number2", "+", value = 10, min = 0, max = 1000, step = 10),
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
            ),
            textOutput("mean_relative_error"),
            textOutput("rmse"),
            conditionalPanel (
              condition = "!output.learned",
              br(),
              fileInput('src_data_loader', 'Choose CSV File to set src data',
                        accept=c('text/csv',
                                 'text/comma-separated-values,text/plain',
                                 '.csv')),
              checkboxInput('src_data_header', 'Header', TRUE),
              radioButtons('src_data_sep', 'Separator', c( Space = '', Comma=',', Semicolon=';' ), ',')
            )
          )
        ),
        tabPanel(
          "First step forward prediction",
          br(),
          conditionalPanel (
            condition = "output.first_step_forward_prediction_TAB_is_visible",
            sliderInput("number_of_first_step_predictions",
                        label = "Number of first step predictions",
                        min = 1, max = 5,
                        value = 1,
                        step = 1
            ),
            textOutput("mean_relative_error_for_test"),
            textOutput("rmse_for_test")
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