#' Title
#'
#' @param data training data as df
#' @param type type of algorithm
#'
#' @return fitted model
#' @export
#'
#' @examples
get.model.factory <- function(data, type, model_weights) {
  start.time <- Sys.time()
  
  # define training control
  train_control <-
    trainControl(
      verbose          = TRUE,
      method           = "cv",
      # K-fold cross validation
      number           = 5,
      repeats = 2,
      summaryFunction = twoClassSummary,
      classProbs = TRUE,
      allowParallel    = TRUE
    )
  
  # use down sampling if sample weights not provided
  if(is.na(model_weights) || length(model_weights) == 0){
    train_control$sampling <- "up"
    model_weights <- NULL
  }
  
  # define the algorithms to try out and compare
  ## boosted ensemble model
  if (type == "xgb") {
    model = train(
      l_state ~ .,
      data      = data,
      trControl = train_control,
      method = "xgbTree",
      metric = "ROC",
      tuneLength = 1
    )
  }
  ## bagged ensemble model
  else if (type == "rf") {
    
    # fitting one tree 
    train_control$method <- "none"
    model <- train(
      l_state ~ .,
      data      = data,
      method    = "ranger",
      na.action = na.omit,
      weights = model_weights,
      metric = "ROC",
      trControl = train_control,
      tuneLength = 1
    )
  }
  
  ## Generalised Linear model
  else if (type == "glm") {
    model <-  train(
      l_state ~ .,
      data = data,
      method = "glmboost",
      na.action = na.omit,
      metric = "ROC",
      trControl = train_control,
      tuneLength = 1
    )
  }
  ## Boostd model
  else if (type == "gbm") {
    model <-  train(
      l_state ~ .,
      data = data,
      method = "gbm",
      na.action = na.omit,
      weights = model_weights,
      metric = "ROC",
      tuneLength = 1,
      trControl = train_control
    )
  }
  
  print(paste0(type, "  algorithm takes ", (Sys.time() - start.time), " seconds"))
  return(model)
}


#' Title
#' takes a model and test data to compute auc values
#' 
#' @param model 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
test_roc <- function(model, data) {
  rc <- roc(data$l_state,
            predict(model, data, type = "prob")[, "Default"])
  return(rc)
}