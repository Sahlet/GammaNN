library(shiny)
library(dygraphs)
library(xts)
library(datasets)
library(GammaNN)

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
    insertUI(
      selector = '#series_graphs_1',
      ui = br()
    )
  }
  
  fields <- reactiveValues(NN = NULL, learned = FALSE, mean_relative_error = 0, mse = 0, learn_button_click_count_mod_2 = 0);
  
  output$mean_relative_error <- renderText({ 
    paste("Мean relative error", round(fields$mean_relative_error * 100, digits = 2), "%");# средняя относительная ошибка
  })
  
  output$rmse <- renderText({
    paste("RMSE", round(fields$mse^0.5, digits = 3));# корень среднеквадратической ошибки
  })
  
  get_table_from_file <- reactive({
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
      
      updateSliderInput(
        session, "training_data",
        label = "Training data",
        min = 1, max = as.integer(nrow(result)*0.8),
        value = as.integer(nrow(result)*0.5),
        step = 1
      );
    }
    
    return(result);
  });
  
  get_src_data_when_not_learned <- reactive({
    result <- NULL;
    if (!is.null(input$src_data_loader) && !is.null(input$src_data_loader$datapath)) {
      result <- read.table(input$src_data_loader$datapath, header=input$src_data_header, sep=input$src_data_sep);
      if (!input$src_data_header) {
        column_names <- as.character(1:ncol(result));
        for (i in 1:length(column_names)) {
          column_names[i] <- paste0("x", column_names[i]);
        }
        colnames(result) <- column_names;
      }
    }
    
    return(result);
  });
  
  output$first_step_forward_prediction_TAB_is_visible <- reactive({
    return(!is.null(fields$NN) && (!is.null(get_table_from_file()) || !is.null(get_src_data_when_not_learned())));
  });
  outputOptions(output, 'first_step_forward_prediction_TAB_is_visible', suspendWhenHidden=FALSE);
  
  observeEvent(input$training_data, {
    updateSliderInput(
      session, "batch_size",
      label = "Batch size",
      min = 1, max = input$training_data,
      value = 1,
      step = 1
    );
  })

  get_table <- reactive({
    
    if (!is.null(fields$NN) && !fields$learned) {
      obj <- get_src_data_when_not_learned();
      if (!is.null(obj)) {
        return (obj);
      } else {
        return(as.data.frame(GammaNN::get_series(fields$NN, 1:GammaNN::get_src_series_length(fields$NN))));
      }
    } else {
      return(get_table_from_file());
    }

    return(NULL);
  });

  output$learn_button_is_visible <- reactive({
    return(!is.null(get_table_from_file()));
  });
  outputOptions(output, 'learn_button_is_visible', suspendWhenHidden=FALSE);
  
  output$learned <- reactive({
    return(fields$learned);
  });
  outputOptions(output, 'learned', suspendWhenHidden=FALSE);
  
  refresh_graphs <- reactive({
    clear_series_graphs_container();
    object_numbers <- ((input$prediction_range)[1] : (input$prediction_range)[2]);
    
    #NN_series is frame
    NN_series <- as.data.frame(GammaNN::get_series(fields$NN, object_numbers));
    
    objects <- data.frame(matrix(0, ncol = ncol(get_table()), nrow = length(object_numbers)));
    
    fields$mean_relative_error <- 0;
    fields$mse <- 0;

    {
      j <- 1;
      for (i in object_numbers) {
        objects[j,] <- get_table()[i,];
        
        for (k in 1:
             #1
             ncol(NN_series)
             ) {
          err <- abs(objects[j,k] - NN_series[j,k]);
          rerr <- abs(err / objects[j,k])
          
          if (!is.na(rerr) && !is.infinite(rerr)) {
            fields$mean_relative_error <- fields$mean_relative_error + rerr;
          }
          
          if (!is.na(err) && !is.infinite(err)) {
            fields$mse <- fields$mse + err*err;
          }
        }
        
        j <- j + 1;
      }
      
      if (
        fields$mean_relative_error != 0 &&
        GammaNN::get_src_series_length(fields$NN) < nrow(get_table()) &&
          (
            (
              GammaNN::get_src_series_length(fields$NN) < (input$prediction_range)[1]
              &&
              (input$prediction_range)[1] <= nrow(get_table())
            )
            ||
            (
              GammaNN::get_src_series_length(fields$NN) < (input$prediction_range)[2]
              &&
              (input$prediction_range)[2] <= nrow(get_table())
            )
            ||
            (
              (input$prediction_range)[1] <= GammaNN::get_src_series_length(fields$NN)
              &&
              nrow(get_table()) <= (input$prediction_range)[2]
            )
          )
      ) {
        
        count <- (
          min(
            (input$prediction_range)[2] - (input$prediction_range)[1] + 1,
            nrow(get_table()) - GammaNN::get_src_series_length(fields$NN),
            (input$prediction_range)[2] - GammaNN::get_src_series_length(fields$NN),
            nrow(get_table()) - (input$prediction_range)[1] + 1
          )
          *
            ncol(get_table())
        );
        
        fields$mean_relative_error <- fields$mean_relative_error / count;
        fields$mse <- fields$mse / count;
          
      }
    }
    
    colnames(objects) <- colnames(get_table());
    
    for (i in 1:ncol(NN_series)) {
      name <- paste0("dygraph_graph", as.character(i));
      local({
        my_i <- i;
        output[[name]] <- renderDygraph ({
          i_series <- list(
            time = object_numbers,
            predicted = NN_series[[my_i]],
            src = objects[[my_i]]
          );
          return(
              dygraph(i_series, main = colnames(NN_series)[my_i])
          );
        });
      })
      
      insert_series_graph(dygraphOutput(name));
    }
    
  });

  prev_NN_exists <- FALSE;
  
  output$NN_exists <- reactive({
    result <- !is.null(fields$NN);

    if (prev_NN_exists != result) {
      prev_NN_exists = result;
      clear_series_graphs_container();
      if (result) {
        updateSliderInput(
          session, "prediction_range",
          label = "Range:",
          min = 1, max = GammaNN::get_src_series_length(fields$NN) + 200,
          value = c(1, GammaNN::get_src_series_length(fields$NN) + 10 + fields$learn_button_click_count_mod_2),
          step = 1
        );
      }
    }

    return(result);
  });
  outputOptions(output, 'NN_exists', suspendWhenHidden=FALSE);
  
  observeEvent(input$prediction_range, {
    if (!is.null(fields$NN)) {
      refresh_graphs();
    }
  })

  observeEvent(input$learn, {
    objects <- data.frame(matrix(0, ncol = ncol(get_table()), nrow = input$training_data));
    
    for (i in 1:input$training_data) {
      objects[i,] <- get_table()[i,];
    }
    colnames(objects) <- colnames(get_table());
    
    hidden <- integer();
    
    if (input$rectangle_hidden) {
      hidden <- rep(input$hidden_layers_width, input$hidden_layers);
    } else {
      hidden <- as.integer(strsplit(input$hidden_layers_text, ',')[[1]]);
    }
    
    fields$NN <-
      GammaNN::learn(
        objects, hidden,
        input$gamma_units, input$trace_size,
        input$eps, input$batch_size, input$random_patterns,
        input$max_epoch_number + input$max_epoch_number2
      );
    
    fields$learned <- TRUE;
    if (fields$learn_button_click_count_mod_2 > 0) {
      fields$learn_button_click_count_mod_2 <- 1
    } else {
      fields$learn_button_click_count_mod_2 <- 0
    }
  })
  
  local_file_path <- paste0(
    getwd(), '/', 
    #session$ns("name")
    "file_buffer"
  )

  observeEvent(input$NN_uploader, {
    if (!is.null(input$NN_uploader) && !is.null(input$NN_uploader$datapath)) {
      file.remove(local_file_path);
      file.copy(from = input$NN_uploader$datapath, to = local_file_path);
    } else {
      return()
    }

    fields$NN <- GammaNN::create_from_file(local_file_path);
    fields$learned <- FALSE;
  })

  output$NN_downloader <- downloadHandler (
    filename = function() {
      return ('Gamma.NN')
    },
    content = function(con) {
      GammaNN::write_to_file(fields$NN, local_file_path)
      file.copy(from = local_file_path, to = con)
    }
  )

})