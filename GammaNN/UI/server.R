library(shiny)
library(dygraphs)
library(datasets)
#library(moments)

primary_statistical_analysis <- function(vec, confidence_level = 0.95) {
  size_vec <- length(vec);
  t_quantile <- qt((1 + confidence_level) / 2, size_vec - 1);
  
  mean_vec <- mean(vec);
  sd_vec <- sd(vec);
  #asymmetry
  skewness_vec <- skewness(vec);
  #excess
  kurtosis_vec <- kurtosis(vec);
  
  mean_deviation <- t_quantile*sd_vec/sqrt(size_vec);
  sd_deviation <- t_quantile*sd_vec/sqrt(2*size_vec);
  skewness_deviation <- t_quantile*sqrt(6*(size_vec - 2)/((size_vec + 1)*(size_vec + 3)));
  kurtosis_deviation <-
    t_quantile*sqrt(
      24*size_vec*(size_vec - 2)*(size_vec - 3)
      /
        (((size_vec + 1)^2)*(size_vec + 3)*(size_vec + 5))
    );
  
  return(c(
    as.character(mean_vec),
    paste0("(", mean_vec - mean_deviation, ", ", mean_vec + mean_deviation, ")"),
    as.character(sd_vec),
    paste0("(", sd_vec - sd_deviation, ", ", sd_vec + sd_deviation, ")", sep = ""),
    as.character(skewness_vec),
    paste0("(", skewness_vec - skewness_deviation, ", ", skewness_vec + skewness_deviation, ")", sep = ""),
    as.character(kurtosis_vec),
    paste0("(", kurtosis_vec - kurtosis_deviation, ", ", kurtosis_vec + kurtosis_deviation, ")", sep = "")
  ));
}

correlation_ratio_test <- function(my_table, points_from_range_, confidence_level = 0.95) {
  my_table_x_range <- range(my_table[[1]]);
  points_from_range <- sort(split(points_from_range_, sapply(points_from_range_, function(arg) {
    if (arg >= my_table_x_range[1] && arg <= my_table_x_range[2]){
      return (1);
    }
    return(0);
  }))$'1');
  
  if (points_from_range[1] != my_table_x_range[1]) points_from_range <- c(my_table_x_range[1], points_from_range);
  points_from_range_length <- length(points_from_range);
  if (points_from_range[points_from_range_length] != my_table_x_range[2]) {
    points_from_range <- c(points_from_range, my_table_x_range[2]);
    points_from_range_length <- points_from_range_length + 1;
  }
  
  ranges <- matrix(nrow = points_from_range_length - 1, ncol = 2);
  
  for(n in 1:(points_from_range_length - 1)) {
    ranges[n,] <- c(points_from_range[n], points_from_range[n + 1]);
  }
  
  my_table_length <- length(my_table[[1]]);
  ranges_length <- points_from_range_length - 1;
  range_numbers <- array(dim = my_table_length);
  
  n <- 1;
  while (n <= my_table_length) {
    if (is.na(my_table[n, 1])){
      range_numbers[n] <- NA;
      n <- n + 1;
      next;
    }
    range_number <- 1;
    while(range_number <= (ranges_length - 1)){
      if (
        (ranges[range_number, 1] <= my_table[n, 1]) &&
        (my_table[n, 1] < ranges[range_number, 2])
      ){
        break;
      }
      range_number <- range_number + 1;
    }
    range_numbers[n] <- range_number;
    n <- n + 1;
  }
  
  averages <- tapply(1:my_table_length, range_numbers, function(my_table_part) {
    mean(my_table[[2]][my_table_part]);
  });
  
  ratio <- (
    sd(sapply(range_numbers, function(range_number){
      averages[as.character(range_number)];
    }))
    /
      sd(my_table[[2]])
  )^2;
  
  result <- list();
  
  result$data.description <- paste("data: ", colnames(my_table)[1], "and", colnames(my_table)[2]);
  
  df1 <- ranges_length - 1;
  df2 <- my_table_length - ranges_length;
  
  result$test.f <- (ratio/df1)/((1 - ratio)/df2);
  result$test.df1 <- df1;
  result$test.df2 <- df2;
  result$test.f_quantile <- qf(confidence_level, df1, df2);
  result$test.p_val <- 1 - pf(result$test.f, df1, df2);
  
  if (result$test.f <= result$test.f_quantile) {#H0
    result$hypothesis.main <- TRUE;
    result$hypothesis.alternative <- FALSE;
    result$hypothesis.description <- "main hypothesis: true ratio is equal to 0";
  } else {#H1
    result$hypothesis.main <- FALSE;
    result$hypothesis.alternative <- TRUE;
    result$hypothesis.description <- "alternative hypothesis: true ratio is not equal to 0";
  }
  
  result$conf.description <- paste(result$conf.level, "percent confidence interval:", result$conf.int);
  
  result$ratio.estimate <- ratio;
  result$ratio.description <- paste("ratio estimate:", result$ratio.estimate);
  
  return (result);
};

lm_coeff_table <- function(summ, confidence_level){
  #summ$coefficients; is: Estimate, Std.Error of estimate (for conf.int),  t_statistic, p_level
  
  row_names <- c("offset", "coeff");
  
  estimate <- summ$coefficient[,1];
  
  std_error <- summ$coefficient[,2];
  
  statistic <- summ$coefficient[,3];
  
  quantile <- rep(qt(confidence_level, summ$fstatistic[3]), times = 2);
  
  H0_estimate_equal_to_0 <- abs(statistic) <= quantile;
  
  p_level <- summ$coefficient[,4];
  
  conf_int <- sapply(1:2, function(i) {
    paste0("(", estimate[i] - std_error[i]*quantile[i], "; ", estimate[i] + std_error[i]*quantile[i], ")")
  });
  
  estimate <- c(
    as.character(estimate[1]),
    as.character(estimate[2])
  );
  
  result <- data.frame(
    row_names,
    estimate,
    std_error,
    statistic,
    quantile,
    H0_estimate_equal_to_0,
    p_level,
    conf_int
  );
  
  colnames(result)[1] <- "";
  colnames(result)[6] <- "H0: estimate is 0";
  
  return (result);
}

shinyServer(function(input, output) {})

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
#   observeEvent(input$insertBtn1, {
#     insertUI(
#       selector = '#placeholder',
#       ui = tags$div(id = 'placeholder2')
#     )
#   })
#   
#   counter <- 1;
#   
#   observeEvent(input$insertBtn, {
#     insertUI(
#       selector = '#placeholder2',
#       ui = textInput(
#         "txt"
#         #paste0("txt", counter)
#         , "Insert some text")
#     )
#     counter <- counter + 1;
#   })
#   
#   observeEvent(input$removeBtn, {
#     removeUI(
#       selector = '#placeholder2',
#       #multiple = TRUE,
#       #immediate = TRUE
#     )
#   })
# 
# })

# shinyServer(function(input, output) {
#   
#   # By declaring get_my_table as a reactive expression we ensure 
#   # that:
#   #
#   #  1) It is only called when the inputs it depends on changes
#   #  2) The computation and result are shared by all the callers 
#   #	  (it only executes a single time)
#   #
#   get_my_table <- reactive({
#     result <- NULL;
#     if (!is.null(input$file1) && !is.null(input$file1$datapath)){
#       result <- read.table(input$file1$datapath, header=input$header, sep=input$sep);
#       if (!input$header) {
#         colnames(result) <- c('x', 'y');
#       }
#     }
#     return(result);
#   });
#   
#   get_my_table_lm <- reactive({
#     my_table <- get_my_table();
#     if (is.null(my_table)) {
#       return(NULL);
#     }
#     
#     return(lm(my_table[[2]] ~ my_table[[1]]));
#   });
#   get_my_table_lm_summ <- reactive({
#     my_table <- get_my_table();
#     if (is.null(my_table)) {
#       return(NULL);
#     }
#     
#     return(summary(get_my_table_lm()));
#   });
#   
#   get_my_table_nlm <- reactive({
#     my_table <- get_my_table();
#     if (is.null(my_table)) {
#       return(NULL);
#     }
#     
#     return(lm(1/my_table[[2]] ~ my_table[[1]]));
#   });
#   get_my_table_nlm_summ <- reactive({
#     my_table <- get_my_table();
#     if (is.null(my_table)) {
#       return(NULL);
#     }
#     
#     return(summary(get_my_table_nlm()));
#   });
#   
#   #* **1.**	Побудову кореляційного поля
#   output$correlation_field <- renderPlot({
#     my_table <- get_my_table();
#     if (is.null(my_table)) {
#       return(NULL);
#     }
#     
#     N <- length(my_table[[1]])
#     predicat_mean <- mean(my_table[[1]]);
#     x_min_max <- range(my_table[[1]]);
#     x <- seq(x_min_max[1] - (x_min_max[2] - x_min_max[1]) / 4, x_min_max[2] + (x_min_max[2] - x_min_max[1]) / 4, 0.1);
#     x_length <- length(x);
#     t_quantile <- qt((1 + input$confidence_level)/2, N - 2);
#     
#     #########################################################################
#     lm_ <- get_my_table_lm();
#     summ <- get_my_table_lm_summ();
#     
#     pl_ <- plot(my_table);
#     grid();
#     abline(lm_, col = "red");
#     
#     y_func <- function(x) { lm_$coefficients[1] + x*lm_$coefficients[2]; }
#     
#     y <- sapply(x, y_func);
#     
#     sigma_sqr <- summ$sigma^2;
#     sigma_k_sqr <- summ$coefficient[2,2]^2;
#     
#     std_errors <- sapply (1:x_length, function(n){ sqrt(sigma_sqr/N + sigma_k_sqr*((x[n] - predicat_mean)^2)) });
#     half_of_conf_interval <- t_quantile*std_errors;
#     half_of_predict_interval <- t_quantile*sqrt(std_errors^2 + sigma_sqr);
#     
#     lines(x, y + half_of_conf_interval, col = "red", lty = 2);
#     lines(x, y - half_of_conf_interval, col = "red", lty = 2);
#     lines(x, y + half_of_predict_interval, col = "red", lty = 4);
#     lines(x, y - half_of_predict_interval, col = "red", lty = 4);
#     
#     #########################################################################
#     #y = 1/(k*x + b)
#     lm_ <- get_my_table_nlm();
#     summ <- get_my_table_nlm_summ();
#     
#     y <- sapply(x, y_func);
#     lines(x, 1/y, col = "blue");
#     
#     sigma_sqr <- summ$sigma^2;
#     sigma_k_sqr <- summ$coefficient[2,2]^2;
#     
#     std_errors <- sapply (1:x_length, function(n){ sqrt(sigma_sqr/N + sigma_k_sqr*((x[n] - predicat_mean)^2)) });
#     half_of_conf_interval <- t_quantile*std_errors;
#     half_of_predict_interval <- t_quantile*sqrt(std_errors^2 + sigma_sqr);
#     
#     lines(x, 1/(y + half_of_conf_interval), col = "blue", lty = 2);
#     lines(x, 1/(y - half_of_conf_interval), col = "blue", lty = 2);
#     lines(x, 1/(y + half_of_predict_interval), col = "blue", lty = 4);
#     lines(x, 1/(y - half_of_predict_interval), col = "blue", lty = 4);
#     
#     return(pl_);
#   });
#   
#   output$sample_size <- renderPrint({
#     my_table <- get_my_table();
#     if (!is.null(my_table)){
#       return(cat(paste("sample size is", length(my_table[[1]]))));
#     }
#     return(NULL);
#   });
#   
#   #і проведення первинного статистичного аналізу окремих ознак об’єкта (точкове та інтервальне оцінювання середнього, середньоквадратичного, коефіцієнтів асиметрії та ексцесу).
#   output$primary_statistical_analysis <- renderTable({
#     my_table <- get_my_table();
#     if (is.null(my_table)) return(NULL);
#     row_names <- c(
#       "mean",
#       "mean conf.int",
#       "sd (standard deviation)",
#       "sd conf.int",
#       "asymmetry",
#       "asymmetry conf.int",
#       "excess",
#       "excess conf.int"
#     );
#     
#     #cat("sample size is", length(my_table[[1]]), "\n\n");
#     
#     result <- data.frame(
#       row_names,
#       primary_statistical_analysis(my_table[[1]], input$confidence_level),
#       primary_statistical_analysis(my_table[[2]], input$confidence_level)
#     );
#     colnames(result) <- c("", colnames(my_table));
#     return (result);
#   });
#   
#   #* 2.1.	знаходження оцінки коефіцієнта кореляції, перевірку його значущості та призначення довірчого інтервалу (у випадку значущості);
#   #* 2.3.	підрахунок рангових коефіцієнтів кореляції Спірмена та Кендалла та перевірку їх значущості.
#   output$correlation_test <- renderTable({
#     #Pearson_correlation_test
#     my_table <- get_my_table();
#     if (is.null(my_table)) return(NULL);
#     #Pearson correlation test
#     cor_test <- cor.test(my_table[[1]], my_table[[2]], conf.level = input$confidence_level, method = "pearson");
#     #cor_test$data.name <- paste(colnames(my_table)[1], "and", colnames(my_table)[2]);
#     ratio_test <- NULL;
#     
#     if (!is.null(my_table) && !is.null(input$set_own_subranges)) {
#       
#       subranges <- c();
#       
#       call_correlation_ratio_test <- TRUE;
#       
#       if (!input$set_own_subranges) {
#         my_table_x_range <- range(my_table[[1]]);
#         if (input$subrange_number <= 1) {
#           subranges <- my_table_x_range;
#         } else {
#           range_length = (my_table_x_range[2] - my_table_x_range[1]) / input$subrange_number;
#           for (n in 0:(input$subrange_number - 1)){
#             subranges <- c(subranges, my_table_x_range[1] + n*range_length);
#           }
#           subranges <- c(subranges, my_table_x_range[2]);
#         }
#       } else {
#         if (!is.null(input$file2_ranges) && !is.null(input$file2_ranges$datapath)){
#           subranges <- read.table(input$file2_ranges$datapath, header=FALSE, sep='')[[1]];
#         } else {
#           call_correlation_ratio_test <- FALSE;
#         }
#       }
#       
#       if (call_correlation_ratio_test)
#         ratio_test <- correlation_ratio_test(my_table, subranges, input$confidence_level);
#     }
#     
#     #Spearman correlation test
#     Sp_cor_test <- cor.test(my_table[[1]], my_table[[2]], conf.level = input$confidence_level, method = "spearman");
#     
#     #Kendall correlation test
#     Kend_cor_test <- cor.test(my_table[[1]], my_table[[2]], conf.level = input$confidence_level, method = "kendall");
#     
#     null_rm <- function(val){
#       if (is.null(val)) return("-");
#       return(val);
#     }
#     
#     row_names <- c(
#       "Pearson's",
#       "corr ratio",
#       "Spearman's",
#       "Kendall's"
#     );
#     
#     estimate <- c(
#       cor_test$estimate,
#       null_rm(ratio_test$ratio.estimate),
#       Sp_cor_test$estimate,
#       Kend_cor_test$estimate
#     );
#     
#     statistic <- c(
#       cor_test$statistic,
#       null_rm(ratio_test$test.f),
#       Sp_cor_test$statistic,
#       Kend_cor_test$statistic
#     );
#     
#     quantile <- c(
#       qt((1 + input$confidence_level)/2, length(my_table[[1]]) - 2),
#       null_rm(ratio_test$test.f_quantile),
#       qt((1 + input$confidence_level)/2, length(my_table[[1]]) - 2),
#       qnorm((1 + input$confidence_level)/2)
#     );
#     
#     H0_estimate_equal_to_0 <- abs(statistic) <= quantile;
#     
#     p_level <- c(
#       cor_test$p.value,
#       null_rm(ratio_test$test.p_val),
#       Sp_cor_test$p.value,
#       Kend_cor_test$p.value
#     );
#     conf_int <- c(
#       paste0("(", cor_test$conf.int[1], "; ", cor_test$conf.int[2], ")"),
#       "-",
#       "-",
#       "-"
#     );
#     
#     result <- data.frame(
#       row_names,
#       estimate,
#       statistic,
#       quantile,
#       H0_estimate_equal_to_0,
#       p_level,
#       conf_int
#     );
#     
#     colnames(result)[1] <- "";
#     colnames(result)[5] <- "H0: estimate is 0";
#     
#     return (result);
#     
#   });
#   
#   #* 2.2.	обчислення коефіцієнта кореляційного відношення та перевірку його значущості;
#   output$min_max <- renderPrint({
#     my_table <- get_my_table();
#     if (is.null(my_table)) return(NULL);
#     
#     return(cat(
#       paste0("[min(", colnames(my_table)[1], "); max(", colnames(my_table)[1], ")]   =   [", min(my_table[[1]]), "; ", max(my_table[[1]]), "]")
#     ));
#   });
#   
#   output$select_file_text <- renderPrint({
#     my_table <- get_my_table();
#     if (is.null(my_table)) return(NULL);
#     
#     return(cat(
#       paste0("Select file with vector of numbers from range [", min(my_table[[1]]), "; ", max(my_table[[1]]), "]")
#     ));
#   });
#   
#   output$lin_regression_description <- renderPrint({
#     my_table <- get_my_table();
#     if (is.null(my_table)){
#       return(NULL);
#     }
#     
#     return(cat(
#       paste0(
#         colnames(my_table)[2], " = coeff*", colnames(my_table)[1], " + offset"
#       )
#     ));
#   });
#   
#   output$lin_regression <- renderTable({
#     my_table <- get_my_table();
#     if (is.null(my_table)) {
#       return(NULL);
#     }
#     return (lm_coeff_table(get_my_table_lm_summ(), input$confidence_level));
#   });
#   
#   output$lin_regression_R_sqr_and_Fstatistic <- renderPrint({
#     my_table <- get_my_table();
#     if (is.null(my_table)){
#       return(NULL);
#     }
#     
#     summ <- get_my_table_lm_summ();
#     
#     return(cat(
#       paste0(
#         "Multiple R-squared: ", summ$r.squared, "\n",
#         "F-statistic: ", summ$fstatistic[1], " on ", summ$fstatistic[2], " and ", summ$fstatistic[3], " DF",  ", p_value: ", 1 - pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3])
#       )
#     ));
#   });
#   
#   output$nlin_regression_description <- renderPrint({
#     my_table <- get_my_table();
#     if (is.null(my_table)){
#       return(NULL);
#     }
#     
#     return(cat(
#       paste0(
#         "1/", colnames(my_table)[2], " = coeff*", colnames(my_table)[1], " + offset"
#       )
#     ));
#   });
#   
#   output$nlin_regression <- renderTable({
#     my_table <- get_my_table();
#     if (is.null(my_table)) {
#       return(NULL);
#     }
#     return (lm_coeff_table(get_my_table_nlm_summ(), input$confidence_level));
#   });
#   
#   output$nlin_regression_R_sqr_and_Fstatistic <- renderPrint({
#     my_table <- get_my_table();
#     if (is.null(my_table)){
#       return(NULL);
#     }
#     
#     summ <- get_my_table_nlm_summ();
#     
#     return(cat(
#       paste0(
#         "Multiple R-squared: ", summ$r.squared, "\n",
#         "F-statistic: ", summ$fstatistic[1], " on ", summ$fstatistic[2], " and ", summ$fstatistic[3], " DF",  ", p_value: ", 1 - pf(summ$fstatistic[1], summ$fstatistic[2], summ$fstatistic[3])
#       )
#     ));
#   });
#   
# })