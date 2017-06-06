library(chron)

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

get_series_by_AGZS_ID <- NULL;
get_the_longest_AGZS_series <- NULL;

{

AGZS_table <- read.table("../Data/agzs_remnants.txt", header=TRUE, sep=',');
max_DAYSALES <- as.numeric(max(AGZS_table$DAYSALES))
min_DAYSALES <- as.numeric(min(AGZS_table$DAYSALES))

normalize_by_min_max <- function(vec, vec_min, vec_max) {
    sapply(vec, function(val) {(val - vec_min) / (vec_max - vec_min) });
}

normalize <- function(vec) {
  vec_max <- as.numeric(max(vec, na.rm = TRUE));
  vec_min <- as.numeric(min(vec, na.rm = TRUE));
  normalize_by_min_max(vec, vec_min, vec_max);
}

normalize_table <- function(table) {
  vec_max <- as.numeric(max(sapply(table, function(vec) {max(vec, na.rm = TRUE)})))
  vec_min <- as.numeric(min(sapply(table, function(vec) {min(vec, na.rm = TRUE)})))
  
  res <- as.data.frame(lapply(table, function(vec) { normalize_by_min_max(vec, vec_min, vec_max); }));
  
  colnames(res) <- colnames(table);
  
  return (res);
}

get_series_by_AGZS_ID <<- function(AGZS_ID) {
  if(is.null(AGZS_table)) return(NULL);
  
  series <- within(AGZS_table[AGZS_table$AGZS_ID == AGZS_ID,], rm(AGZS_ID));
  #series <- within(series[order(series$REMNANTS_DATE),], rm(REMNANTS_DATE))
  series <- series[order(series$REMNANTS_DATE),];
  series$DAYSALES <- normalize(series$DAYSALES);
  # series[["DAYSALES"]] <- as.numeric(series[["DAYSALES"]]);
  # for (i in 1:nrow(series)) {
  #   series$DAYSALES[i] <- ((series$DAYSALES[i] - min_DAYSALES) / (max_DAYSALES - min_DAYSALES));
  # }
    
  rownames(series) <- NULL;
  
  return (series);
}

get_the_longest_AGZS_series <<- function(AGZS_series) {
  if (class(AGZS_series) != class(data.frame())) return(NULL);
  if (nrow(AGZS_series) <= 1) return(AGZS_series);
  
  res_start <- 1;
  res_length <- 0;
  
  cur_start <- 1;
  cur_length <- 0;
  
  dtparts = t(as.data.frame(strsplit(as.character(AGZS_series$REMNANTS_DATE), 'T')));
  rownames(dtparts) <- NULL;
  thetimes = chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'));
  
  eps <- 0.05
  
  max_i <- nrow(AGZS_series) - 1;
  for (i in 1:max_i) {
    difference <- unclass(thetimes[i + 1] - thetimes[i])[[1]];
    if (
      difference > 1 + eps
      ||
      difference < 1 - eps
    ) {
      cur_start <- i + 1;
      cur_length <- 0;
    } else {
      cur_length <- cur_length + 1;
      if (res_length < cur_length) {
        res_start <- cur_start;
        res_length <- cur_length;
      }
    }
  }
  
  return (AGZS_series[res_start:(res_start + res_length),]);
}

}

write_AGZS <- function(ID, file_path = "../Data") {
  file_path <- paste0(file_path, "/AGZS_", ID, ".txt");
  res <- within(get_the_longest_AGZS_series(get_series_by_AGZS_ID(ID)), rm(REMNANTS_DATE));
  res$DAYSALES <- normalize(res$DAYSALES);
  colnames(res) <- paste0("DAYSALES_", ID);
  write.csv(res, file_path, row.names = FALSE, quote = FALSE);
}

normalize_table_in_file <- function(path) {
  write.csv(normalize_table(read.table(path, header=TRUE, sep=',')), path, row.names = FALSE, quote = FALSE);
}

wp_mean_24 <- read.table("../Data/mean_target_24.csv", header=TRUE, sep=',');

get_wp_correlation_coeffs <- function() {
  if (ncol(wp_mean_24) <= 1) return(c());
  
  corrs <- numeric();
  wp_mean_24_colnames <- colnames(wp_mean_24);
  names <- character();
  
  for (i in 1:(ncol(wp_mean_24) - 1)) {
    for (j in (i + 1):ncol(wp_mean_24)) {
      corrs <- c(corrs, cor(wp_mean_24[[i]], wp_mean_24[[j]]));
      names <- c(names, paste0(wp_mean_24_colnames[i], " and ", wp_mean_24_colnames[j]));
    }
  }
  
  res <- data.frame(corrs);
  
  rownames(res) <- names;
  
  return (res);
}