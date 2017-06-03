library(shiny)
library(dygraphs)
library(xts)
library(datasets)
library(GammaNN)

mean_by_n <- function(vec, n) {
  res <- vec;
  length(res) <- 0;
  i <- 1;
  while(i <= length(vec)) {
    next_i <- i + min(length(vec) - i, n - 1);
    res <- c(res, mean(vec[i:next_i]));
    i <- next_i + 1;
  }
  
  return(res);
}

mean_by_n_for_frame <- function(table, n) {
  res <- list();
  for (i in 1:ncol(table)) {
    res[[i]] <- mean_by_n(table[[i]], n);
  }
  
  res <- as.data.frame(res);
  colnames(res) <- colnames(table);
  
  return(res);
}

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
  
  fields <- reactiveValues(NN = NULL, learned = FALSE);
  
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
      return(as.data.frame(GammaNN::get_series(fields$NN, 1:GammaNN::get_src_series_length(fields$NN))));
    } else {
      return(get_table_from_file());
    }

    return(NULL);
  });

  output$learn_button_is_visible <- reactive({
    return(!is.null(get_table_from_file()));
  });
  outputOptions(output, 'learn_button_is_visible', suspendWhenHidden=FALSE);
  
  refresh_graphs <- reactive({
    clear_series_graphs_container();
    object_numbers <- ((input$prediction_range)[1] : (input$prediction_range)[2]);
    
    #NN_series is frame
    NN_series <- as.data.frame(GammaNN::get_series(fields$NN, object_numbers));
    
    objects <- data.frame(matrix(0, ncol = ncol(get_table()), nrow = length(object_numbers)));

    {
      j <- 1;
      for (i in object_numbers) {
        objects[j,] <- get_table()[i,];
        j <- j + 1;
      }
    }
    colnames(objects) <- colnames(get_table());
    
    for (i in 1:ncol(NN_series)) {
      name <- paste0("dygraph_graph", as.character(i));
      local({
        my_i <- i;
        output[[name]] <- renderDygraph ({
          i_series <- list(time = object_numbers, predicted = NN_series[[my_i]]);
          if (fields$learned) {
            i_series$src = objects[[my_i]];
          }
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
          min = 1, max = GammaNN::get_series_length(fields$NN) + 500,
          value = c(1, GammaNN::get_series_length(fields$NN)),
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
    
    fields$NN <-
      GammaNN::learn(
        objects, rep(input$hidden_layers_width, input$hidden_layers),
        input$gamma_units, input$trace_size,
        input$eps, input$batch_size, input$random_patterns,
        input$max_epoch_number
      );
    
    fields$learned <- TRUE;
  })
  
  local_file_path <- paste0(
    getwd(), '/', 
    #session$ns("name")
    "file_buffer"
  )

  observeEvent(input$NN_uploader, {
    if (!is.null(input$NN_uploader) && !is.null(input$NN_uploader$datapath)) {
      file.copy(from = input$NN_uploader$datapath, to = local_file_path)
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