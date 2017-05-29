library(shiny)
library(dygraphs)
#shinyUI(

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
          checkboxInput('header', 'Header', FALSE),
          radioButtons('sep', 'Separator', c( Space = '', Comma=',', Semicolon=';' ), ''),
          br(),
          br(),
          sliderInput("training_data_percent", "training data percent",
                      min = 30, max = 80, value = 60, step= 1),
          actionButton("learn", "Learn")
        ),
        tabPanel(
          "Model Serialization",
          br(),
          fileInput('NN_uploader', 'Upload GammaNN File'),
          downloadButton('NN_downloader', 'Download GammaNN File')
        ),
        tabPanel(
          "Prediction",
          br(),
          actionButton("next", "Next")
        )
      )
    ),
    mainPanel(
      tags$div(id = 'placeholder')
    )
  )

)

#)