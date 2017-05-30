library(shiny)
library(dygraphs)
library(datasets)

shinyServer(function(input, output, session) {
  insertUI(
    selector = '#placeholder',
    ui = tags$div(id = 'series_graphs')
  );

  clear_series_graphs_container <- function() {
    removeUI(
      selector = '#series_graphs_1' #,
      #multiple = TRUE,
      #immediate = TRUE
    );
    insertUI(
      selector = '#series_graphs',
      ui = tags$div(id = 'series_graphs_1')
    );
  };
  clear_series_graphs_container();

  insert_series_graph <- function(graph) {
    insertUI(
      selector = '#series_graphs_1',
      ui = graph
    )
  }

  get_table <- reactive({
    result <- NULL;
    if (!is.null(input$CSV_loader) && !is.null(input$CSV_loader$datapath)) {
      result <- read.table(input$CSV_loader$datapath, header=input$header, sep=input$sep);
      if (!input$header) {
        column_names <- as.character(1:ncol(result));
        for (i in 1:length(column_names)) {
          column_names[i] <- paste0("x", column_names[i]);
        }
        colnames(result) <- column_names;
      }
    }

    return(result);
  });

  output$learn_button_is_visible <- reactive({
    return(!is.null(get_table()));
  });
  outputOptions(output, 'learn_button_is_visible', suspendWhenHidden=FALSE);

  fields <- reactiveValues(NN = NULL);

  prev_NN_exists <- FALSE;

  output$NN_exists <- reactive({
    result <- !is.null(fields$NN);

    if (prev_NN_exists != result) {
      prev_NN_exists = result;
      clear_series_graphs_container();
      if (!result) {
      } else {
        series_graphs <- list();
        length(series_graphs) <- GammaNN::get_obj_dimention(fields$NN);
        for (i in 1:length(series_graphs)) {
          name <- paste0("dygraph_graph", as.character(i));
          output[[name]] <- renderDygraph ({
            i_series_name <- GammaNN::get_series_name(fields$NN, i)

            object_numbers <- (input$prediction_range)[1] : (input$prediction_range)[2];
            NN_series <- GammaNN::get_series(fields$NN, i, object_numbers)

            if (is.null(get_table()) || (ncol(get_table()) < (input$prediction_range)[1])) {
              i_series <- cbind(NN_series);
              return(
                dygraph(i_series, main = i_series_name)# %>%
                #dySeries(c("lwr", "fit", "upr"), label = "Deaths") #%>%
                #dyOptions(drawGrid = input$showgrid)
              )
            } else {
              i_series <- cbind(NN_series, get_table()[object_numbers,i]);
              return(
                dygraph(i_series, main = i_series_name) %>%
                dySeries(c(i_series_name, "src"), label = "1231231qwdsdc2 Deaths")
              )
            }
          });

          series_graphs[[i]] <- graph;
          insert_series_graph(dygraphOutput(name));
        }
        fields$series_graphs <- series_graphs;

        updateSliderInput(session, "prediction_range",
                          label = "Range:",
                          min = 1, max = GammaNN::get_series_length(fields$NN) + 500,
                          value = c(1, GammaNN::get_series_length(fields$NN)),
                          step = 1
        );
      }
    }

    return(result);
  });
  outputOptions(output, 'NN_exists', suspendWhenHidden=FALSE);

  observeEvent(input$learn, {
    fields$NN <- GammaNN::learn(get_table(), rep(input$hidden_layers_width, input$hidden_layers), input$gamma_units, input$trace_size);
  })

  observeEvent(input$NN_uploader, {
    str <- NULL;
    if (!is.null(input$NN_uploader) && !is.null(input$NN_uploader$datapath)) {
      str <- readChar(input$NN_uploader$datapath, input$NN_uploader$size);
    }
    if (is.null(str)) return();

    fields$NN <- GammaNN::to_GammaNN(str);
  })

  output$NN_downloader <- downloadHandler (
    filename = function() {
      return ('Gamma.NN')
    },
    content = function(con) {
      writeChar(GammaNN::to_str(fields$NN), con)
    }
  )

})

# shinyServer(function(input, output) {
# 
#   predicted <- reactive({
#     hw <- HoltWinters(ldeaths)
#     predict(hw, n.ahead = input$months,
#             prediction.interval = TRUE,
#             level = as.numeric(input$interval))
#   })
# 
#   output$dygraph <- renderDygraph({
#     dygraph(predicted(), main = "Predicted Deaths/Month") %>%
#       dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
#       dyOptions(drawGrid = input$showgrid)
#   })
#   
#   output$members <- renderDygraph({
#       dygraph(predicted(), main = "Predicted Deaths/Month")
#     })
# 
#   observeEvent(input$Btn, {
#       insertUI(
#         selector = '#placeholder',
#         ui = dygraphOutput("members")
#       );
#   })
# 
# })