
#' Title loads data from a given file path
#' 
#' @param path the path where data is located
#' @param seperator the seperator used in the data
#'
#' @return the data frame object
#' @export
#'
#' @examples
get.data <- function(path, seperator){
  
  df <- read.csv(
    file = path,
    header = TRUE,
    sep = seperator,
    quote = "\"'",
    row.names=NULL,
    na.strings = c("NA", "NaN", " ", "", "None"),
    stringsAsFactors = FALSE
  )
  
  print(paste0("Data loaded from ", path))
  return(df)
}


#' Title
#' drop columns with near zero variance and above a certain amount of missin values
#' 
#' @param df 
#' @param allowed.NA.level 
#'
#' @return
#' @export
#'
#' @examples
drop.columns <- function(df, allowed.NA.level){
  # keep the columns beonf the threshold missing values
  df <- df[, colSums(is.na(df)) <= allowed.NA.level * nrow(df)]
  
  # drop the near zero variance columns
  nzv_cols <- nearZeroVar(df[, names(df) != "l_state"])
  if(length(nzv_cols) > 0){
    df <- df[, -nzv_cols]
  }
  
  # consider the ones only with the annual income present. For a credit score, this is a strong
  # indicator than anything else.
  df <- subset(df, !is.na(annual_inc))
  
  # dropping the id column
  df$id <- NULL
  df$emp_title <- NULL
  
  return(df)
}


#' Title
#' drop the numerical features which are highly correlated
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
drop.correlated.features <- function(df, threshold){
  # get the numeric columns only
  nums <- sapply(df, is.numeric)
  df <- df[ , nums]
  df <- df[complete.cases(df), ]
  
  # plot the correlation
  print(ggcorr(df, geom = "circle", nbreaks = 5,   angle = -45))
  
  # calculate correlation matrix
  correlationMatrix <- cor(df)
  
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=threshold)
  highlyCorrelated = sort(highlyCorrelated)
  
  df <- df[, c(highlyCorrelated)]
  
  # indices of the highly correlated features
  return(colnames(df))
}

#' Title
#' does fancy stuff on the individual columns
#' @param df the input dataframe
#'
#' @return feature augmented dataframe
feature.generate <- function(df){
  
  # change the earliest_cr_line to a new feature as years since that date
  # current year is year difference 1
  df$mnts_since_earliest_cr_line <-  as.integer(format(Sys.Date(), "%Y")) - 
    as.integer(str_sub(df$earliest_cr_line, -4)) + 1
  
  # convert the characters to factors
  character_vars <- lapply(df, class) == "character"
  df[, character_vars] <- lapply(df[, character_vars], as.factor)
  
  # change this to numeric
  df$mths_since_last_delinq <- as.integer(df$mths_since_last_delinq)
  
  return(df)
}

#' Title
#' view the data plotted as an output file
#' @param df  the df having the algo performance numbers
#' @param name the name of file to generat
#'
#' @return
#' @export
#'
#' @examples
view.data <- function(model_list_roc){
  results_list_roc <- list(NA)
  num_mod <- 1
  
  for(the_roc in model_list_roc){
    results_list_roc[[num_mod]] <- 
      data_frame(tpr = the_roc$sensitivities,
                 fpr = 1 - the_roc$specificities,
                 model = names(model_list)[num_mod])
    
    num_mod <- num_mod + 1
  }
  # combine the resuts 
  results_df_roc <- bind_rows(results_list_roc)
  custom_col <- c("red", "blue", "cyan", "black")
  
  # plot it
  ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
    geom_line(aes(color = model, linetype = model), size = 1) +
    scale_color_manual(values = custom_col) +
    geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
    theme_bw(base_size = 18)
}
  
