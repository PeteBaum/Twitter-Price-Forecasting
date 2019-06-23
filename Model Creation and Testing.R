##############################################

# Script that uses the processed tweet and price data to create a forecast model

#############################################

########### Load packages ###########
Sys.setenv(LANG = "en")

library(keras)
library(abind)
library(xgboost)
library(rlist)

########## initialize functions ##########

prepare_input_data <- function(path = paste0(getwd(), "\\data\\model input\\"),  tweets_per_obs = 100, word_count = 100, 
                               train_percentage = 0.6, val_percentage = 0.2, test_percentage = 0.2)
{
  load(paste0(path, "\\input_matrices.RData"))
  load(paste0(path, "\\input_prices.RData"))
  faulty_elements <- data.frame()
  for(i in 1:length(inputs))
  {
    if(ncol(inputs[[i]]) != word_count || nrow(inputs[[i]]) != tweets_per_obs) {faulty_elements <- rbind(faulty_elements, i)}
  }
  inputs <- list.remove(inputs, faulty_elements[,1])
  price_data <- price_data[-faulty_elements[,1],]
  
  model_data <- abind(inputs, along = 3)
  model_data <- unname(model_data)
  model_data <- aperm(model_data, c(3,2,1))
  model_data[is.na(model_data)] <- 0
  model_data[model_data>1] <- 1
  
  train_size <- floor(nrow(price_data)*train_percentage)
  val_size <- floor(nrow(price_data)*val_percentage)
  test_size <- floor(nrow(price_data)*test_percentage)
  
  
  x_train <- model_data[1:train_size,,]
  x_val <- model_data[(train_size+1):(train_size+val_size),,]
  x_test <- model_data[(train_size+val_size+1):(train_size+val_size+test_size),,]
  
  
  label <- 1:nrow(price_data)
  label <- ifelse(price_data$Close>price_data$Open, 1, 0)
  label <- as.numeric(label)
  y_train <- label[1:train_size]
  y_val <- label[(train_size+1):(train_size+val_size)]
  y_test <- label[(train_size+val_size+1):(train_size+val_size+test_size)]
  
  x_train <- array_reshape(x_train, c(train_size, tweets_per_obs  * word_count))
  x_val <- array_reshape(x_val, c(val_size, tweets_per_obs  * word_count))
  x_test <- array_reshape(x_test, c(test_size, tweets_per_obs  * word_count))
  
  data_and_labels <- list()
  data_and_labels[[1]] <- x_train
  data_and_labels[[2]] <- y_train
  data_and_labels[[3]] <- x_val
  data_and_labels[[4]] <- y_val
  data_and_labels[[5]] <- x_test
  data_and_labels[[6]] <- y_test
  return(data_and_labels)
}


neural_network_gridsearch <- function(data_and_labels = data_and_labels, tweets_per_obs = 100, word_count = 100, 
                                      layers = c(1,2,3), dropout = c(0.2, 0.5, 0.7), regularizer = c(0.1, 0.001, 0.0001), learning_rate = c(0.1, 0.001, 0.0001), 
                                      nodes_per_layer = c(16,32,64), epochs = 200, batch_size = 100)
{
  x_train <- data_and_labels[[1]]
  y_train <- data_and_labels[[2]]
  x_val <- data_and_labels[[3]]
  y_val <- data_and_labels[[4]]
  x_test <- data_and_labels[[5]]
  y_test <- data_and_labels[[6]]
  
  parameters <- expand.grid(layers, dropout,regularizer, learning_rate, nodes_per_layer)
  names(parameters) <- c("layers", "dropout", "regularizer", "learning_rate", "nodes_per_layer")
  parameters$train_accuracy <- NA
  parameters$val_accuracy <- NA
  
  for(i in 1:nrow(parameters))
  {
    if(parameters$layers[i] == 1)
    {
      network <- keras_model_sequential() %>%
        layer_dense(units = parameters$nodes_per_layer[i], activation = "relu", input_shape = (word_count*tweets_per_obs), kernel_regularizer = regularizer_l2(parameters$regularizer[i])) %>%
        layer_dense(units = 1, activation = "sigmoid")
      
    } else if (parameters$layers[i] == 2) {
      network <- keras_model_sequential() %>%
        layer_dense(units = parameters$nodes_per_layer[i], activation = "relu", input_shape = (word_count*tweets_per_obs), kernel_regularizer = regularizer_l2(parameters$regularizer[i])) %>%
        layer_dropout(rate = parameters$dropout[i]) %>%
        layer_dense(units = parameters$nodes_per_layer[i], activation = "relu", kernel_regularizer = regularizer_l2(parameters$regularizer)) %>%
        layer_dense(units = 1, activation = "sigmoid")
    } else {
      network <- keras_model_sequential() %>%
        layer_dense(units = parameters$nodes_per_layer[i], activation = "relu", input_shape = (word_count*tweets_per_obs), kernel_regularizer = regularizer_l2(parameters$regularizer[i])) %>%
        layer_dropout(rate = parameters$dropout[i]) %>%
        layer_dense(units = parameters$nodes_per_layer[i], activation = "relu", kernel_regularizer = regularizer_l2(parameters$regularizer)) %>%
        layer_dropout(rate = parameters$dropout[i]) %>%
        layer_dense(units = parameters$nodes_per_layer[i], activation = "relu") %>%
        layer_dense(units = 1, activation = "sigmoid")
    }

    
    network %>% compile(
      optimizer =  optimizer_adam(lr = parameters$learning_rate[i]),
      loss = "binary_crossentropy",
      metrics = c("accuracy")
    )
    
    history <- network %>% fit(
      x_train,
      y_train,
      epochs = epochs,
      batch_size = batch_size,
      validation_data = list(x_val, y_val))
    
    results <- as.data.frame(history[[2]])
    parameters$train_accuracy[i] <- results$acc[epochs]
    parameters$val_accuracy[i] <- results$val_accacc[epochs]
  }
  
  return(parameters)
}



gbm_gridsearch <- function(data_and_labels = data_and_labels,   learning_rate = as.factor(c(0.004, 0.01, 0.02)), subsample = as.factor(c(0.25, 0.5, 0.75)),
                           row_subsample = as.factor(c(0.2, 0.4, 0.6)), depth = as.factor(c(8,10,12)))
{
  x_train <- data_and_labels[[1]]
  y_train <- data_and_labels[[2]]
  x_val <- data_and_labels[[3]]
  y_val <- data_and_labels[[4]]
  x_test <- data_and_labels[[5]]
  y_test <- data_and_labels[[6]]
  
  parameters <- expand.grid(learning_rate, subsample, row_subsample, depth)
  names(parameters) <- c("learning_rate", "Obs.Subsample", "row_subsample", "Depth")
  parameters$train_accuracy <- NA
  parameters$val_accuracy <- NA
  
  for(i in 1:length(parameters$learning_rate))
  {
    bossting.model <- xgboost(data = x_train, label = y_train, max.depth = parameters$Depth[i], eta = parameters$learning_rate[i], nthread = 4, nround = 100, objective = "binary:logistic", colsample_bytree = parameters$row_subsample[i], subsample = parameters$Obs.Subsample[i])
    parameters$test_accuracy[i] <- 1-min(bossting.model$evaluation_log$train_error)
    pred <- predict(bossting.model, x_val)
    accuracy <- mean(as.numeric(pred > 0.5) == y_val)
    parameters$val_accuracy[i] <- accuracy
  }
  
  return(parameters)
}


########## create models ############
data_and_labels <- prepare_input_data()

nn_results <- neural_network_gridsearch()

gbm_results <- gbm_gridsearch()