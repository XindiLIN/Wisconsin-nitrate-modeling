library(tidymodels)
library(tidyverse)
library(ranger)
library(SuperLearner)
library(sf)
library(dplyr)
library(ggplot2)
library(WeightIt)
library(GpGp)
library(Metrics)
library(rnaturalearth)
library(ggplot2)
library(see)
library(yardstick)
library(xgboost)
library(glmnet)
library(kernlab)
library(glmnet)
library(numDeriv)
library(MASS)
library(tigris)
library(rnaturalearthdata)
library(e1071)
library(broom)
library(stringr) # For str_remove_all
library(gratia) # gam plot smooth term
library(ggeffects)
library(caret)
library(recipes)
library(GpGp)
library(ranger)
library(SuperLearner)
library(pROC)
library(yardstick)
library(MLmetrics)
library(tidyr)



back_trans <- function(x) exp(x) - 0.5
log10_trans <- function(x) log10(exp(x) + 0.5)

# The reason to use ranger is because ranger can get the cross-validated residual for the Gaussian process estimation
# we note that for 3-d map, we do not need the leave-one-out kriging
nitrate_prediction_SL <- function(data,data_test = NULL, SL.library = c("SL.lm","SL.ranger"), response_name = "logconcentration_plus_median",
                                  OR_covariate_names = c("logWellDepth","crop_type_combine","drainagecl","precipitation","cafolog","StaticLevel")){
  # formula_obj <- as.formula(paste(response_name, "~", paste(OR_covariate_names, collapse = " + ")))
  SL.library <-match.arg(SL.library)
  SL_obj <- SuperLearner(Y = data[,response_name], X = data[,OR_covariate_names], family = gaussian(),
                            SL.library = SL.library)
  
  gpfit <- GpGp::fit_model(data[,response_name] - SL_obj$Z[,1],
                              locs = data[,c("longitude", "latitude")],
                              covfun_name = "matern_sphere", convtol = 1e-03)
  
  pred <- SL_obj$library.predict
  
  if(is.null(data_test)){
    krige_values_test <- NULL
    pred_test <- NULL
  } else {
    pred_test <- predict.SuperLearner(SL_obj, newdata = data_test)$pred
    krige_values_test <- GpGp::predictions(fit = gpfit, 
                                                locs_pred = data_test[,c("longitude", "latitude")], 
                                                X_pred = rep(1,nrow(data_test)))
  }
  
  
  return(list(SL_obj=SL_obj, gpfit=gpfit,
              pred = pred, pred_test = pred_test, 
              krige_values_test = krige_values_test,
              data = data,
              data_test = data_test))
}

nitrate_prediction_ranger <- function(data,data_test = NULL, response_name = "logconcentration_plus_median",
                                      OR_covariate_names = c("logWellDepth","crop_type_combine","drainagecl","precipitation","cafolog","StaticLevel")){
  
  # 1. construct fitting formula
  formula_obj <- as.formula(paste(response_name, "~", paste(OR_covariate_names, collapse = " + ")))
  
  # 2. fit ranger model
  ranger_obj <- ranger(formula_obj, data = data)
  
  # 3. 
  gpfit <- GpGp::fit_model(data[,response_name] - ranger_obj$predictions,
                           locs = data[,c("longitude", "latitude")],
                           covfun_name = "matern_sphere", convtol = 1e-03)
  
  pred <- predict(ranger_obj, data)$predictions
  
  if(is.null(data_test)){
    krige_values_test <- NULL
    pred_test <- NULL
  } else {
    pred_test <- predict(ranger_obj, data_test)$predictions
    krige_values_test <- GpGp::predictions(fit = gpfit, 
                                           locs_pred = data_test[,c("longitude", "latitude")], 
                                           X_pred = rep(1,nrow(data_test)))
  }
  
  
  return(list(ranger_obj=ranger_obj, gpfit=gpfit,
              pred = pred, pred_test = pred_test, 
              krige_values_test = krige_values_test,
              data = data,
              data_test = data_test))
  
}
  
nitrate_prediction_gam <- function(data,data_test = NULL,response_name = "logconcentration_plus_median",
                                  OR_covariate_names = c("logWellDepth","crop_type_combine","drainagecl","precipitation","cafolog","StaticLevel")){
  # 1. Separate categorical from continuous for formula construction
  categorical_vars <- c("crop_type_combine", "drainagecl")
  continuous_vars <- setdiff(OR_covariate_names, categorical_vars)
  
  # 2. Build the formula: s() for continuous, raw for categorical
  smooth_terms <- paste0("s(", continuous_vars, ")", collapse = " + ")
  linear_terms <- paste0(categorical_vars, collapse = " + ")
  
  full_formula <- as.formula(paste(response_name, "~", smooth_terms, "+", linear_terms))
  
  # 3. Ensure data types are correct (mgcv needs Factors for categorical)
  data <- data %>%
    mutate(across(all_of(categorical_vars), as.factor))
  
  # 4. Fit the model
  gam_obj <- mgcv::gam(full_formula, data = data)
  
  gpfit <- GpGp::fit_model(gam_obj$residuals,
                           locs = data[,c("longitude", "latitude")],
                           covfun_name = "matern_sphere", convtol = 1e-03)
  
  pred <- gam_obj$fitted.values
  
  if(is.null(data_test)){
    krige_values_test <- NULL
    pred_test <- NULL
  } else {
    data_test <- data_test %>%
      mutate(across(all_of(categorical_vars), as.factor))
    pred_test <- predict(gam_obj,data_test)
    krige_values_test <- GpGp::predictions(fit = gpfit, 
                                           locs_pred = data_test[,c("longitude", "latitude")], 
                                           X_pred = rep(1,nrow(data_test)))
  }
  
  
  return(list(gam_obj=gam_obj, gpfit=gpfit,
              pred = pred, pred_test = pred_test, 
              krige_values_test = krige_values_test,
              data = data,
              data_test = data_test))
}



nitrate_prediction_xgboost<- function(data,data_test = NULL,response_name = "logconcentration_plus_median",
                                      OR_covariate_names = c("logWellDepth","crop_type_combine","drainagecl","precipitation","cafolog","StaticLevel")){
  
  # 1. Define and prep the recipe on the TRAINING data
  rec <- recipe(~ ., data = data[, OR_covariate_names]) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep() # This "fits" the preprocessor
  
  # 2. Apply the *same* fitted recipe to both datasets
  design_matrix <- as.matrix(bake(rec, new_data = data[, OR_covariate_names]))
  
  
  xgboost_obj <- xgboost(data = design_matrix, label = data[,response_name],nrounds = 250, early_stopping_rounds = 50)
  
  pred <- predict(xgboost_obj,design_matrix)
  
  
  # we want to get the cross-valied residuals to fit the GP
  # dtrain does not support `data` to be data.frame
  dtrain <- xgb.DMatrix(data = design_matrix, label = data[,response_name])
  
  # Run Cross-Validation to get the residuals
  # When the nfold is large, become very slow
  loocv_model <- xgb.cv(
    # params = params,
    data = dtrain,
    nrounds = 250,
    early_stopping_rounds = 50,
    # monotone_constraints = c(0, -1, rep(0,15)),
    nfold = 10,                 # Key step for LOOCV
    prediction = TRUE,         # Important: tells the function to return predictions
    verbose = 0                # Suppress progress messages
  )
  
  # we can also directly using the residual but not the cross-validation prediction error
  gpfit <- GpGp::fit_model( # data[,response_name] - loocv_model$pred,
                           data[,response_name] - predict(xgboost_obj, design_matrix),
                           locs = data[,c("longitude", "latitude")],
                           covfun_name = "matern_sphere")
  
  
  
  if(is.null(data_test)){
    krige_values_test_RKHS <- NULL
    pred_test <- NULL
  } else {
    design_matrix_test <- as.matrix(bake(rec, new_data = data_test[, OR_covariate_names]))
    pred_test <- predict(xgboost_obj, design_matrix_test)
    krige_values_test <- GpGp::predictions(fit = gpfit, 
                                           locs_pred = data_test[,c("longitude", "latitude")], 
                                           X_pred = rep(1,nrow(data_test)))
  }
  
  
  return(list(xgboost_obj=xgboost_obj, gpfit=gpfit,
              pred = pred, pred_test = pred_test, 
              krige_values_test = krige_values_test,
              data = data,
              data_test = data_test))
  
  
  
}

nitrate_prediction_svm <- function(data,data_test = NULL, tunning = FALSE, response_name = "logconcentration_plus_median",
                                   OR_covariate_names = c("logWellDepth","crop_type_combine","drainagecl","precipitation","cafolog","StaticLevel")){
  
  
  # 1. Define and prep the recipe on the TRAINING data
  rec <- recipe(~ ., data = data[, OR_covariate_names]) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep() # This "fits" the preprocessor
  
  # 2. Apply the *same* fitted recipe to both datasets
  design_matrix <- bake(rec, new_data = data[, OR_covariate_names])
  
  if(tunning){
    gamma_default <- dim(design_matrix)[2]
    svm_auto <- best.svm(x = design_matrix, y = data[,response_name], type='nu-regression', gamma = gamma_default*c(0.25,0.5,1,1.5,2,4,8), tunecontrol = tune.control(cross = 5))
  } else {
    svm_auto <- svm(x = design_matrix, y = data[,response_name], type='nu-regression')
  }
  # svm_auto <- svm(x = design_matrix, y = data$logconcentration_plus_median, type='nu-regression')
  # svm_auto <- best.svm(x = design_matrix, y = data$logconcentration_plus_median, type='nu-regression', gamma = (1/20)*c(0.5,1,1.5,2,4,8), tunecontrol = tune.control(cross = 5))
  # best.svm(x = design_matrix, y = data$logconcentration_plus_median, type='nu-regression')
  
  gpfit_RKHS = GpGp::fit_model(svm_auto$residuals,
                               locs = data[,c("longitude", "latitude")],
                               covfun_name = "matern_sphere")
  
  pred <- predict(svm_auto, newdata = design_matrix)
  if(is.null(data_test)){
    krige_values_test_RKHS <- NULL
    pred_test <- NULL
  } else {
    design_matrix_test   <- bake(rec, new_data = data_test[, OR_covariate_names])
    pred_test <- predict(svm_auto, newdata = design_matrix_test)
    krige_values_test_RKHS <- GpGp::predictions(fit = gpfit_RKHS, 
                                                locs_pred = data_test[,c("longitude", "latitude")], 
                                                X_pred = rep(1,nrow(data_test)))
  }
  
  
  return(list(svm=svm_auto, gpfit=gpfit_RKHS,
              pred = pred, pred_test = pred_test, 
              krige_values_test = krige_values_test_RKHS,
              design_matrix = design_matrix,
              design_matrix_test = design_matrix_test))
}



split_nitrate_data <- function(data, p = 0.7, seed = 1998){
  set.seed(seed)
  data_full <- data
  # train_indices <- sample(nrow(data_full), nrow(data_full) * 0.7) # we would replace this with a stratified sample splitting
  # we partition to balance the distribution of nitrate
  train_indices <- createDataPartition(data$logconcentration_plus_median, list = FALSE, p = p) # from caret package
  data <- data_full[train_indices,]
  data_test <- data_full[-train_indices,]
  return(list(data=data, data_test=data_test))
}


load_nitrate_data <- function(file_path = "/Users/xindilin/Desktop/2024 summer/groundwater_pesticide/data/data_Nitrate_with_covar_median_well.csv", zero_inflated = FALSE){
  command <- paste("brctl download", shQuote(file_path))
  system(command)
  data <- read.csv(file_path)
  # Assuming 'data' is your data frame with a 'County' column.
  # Initialize the new 'area' column.
  data$area <- ""
  
  ## Northern Districts
  indices_northwest <- data$County %in% c("Douglas", "Bayfield", "Ashland", "Iron", 
                                          "Washburn", "Sawyer", "Burnett", "Polk", 
                                          "Barron", "Rusk", "St. Croix", "Dunn", 
                                          "Chippewa", "Pierce", "Eau Claire")
  
  data[indices_northwest, "area"] <- "Northwest"
  
  indices_northCentral <- data$County %in% c("Price", "Vilas", "Oneida", "Lincoln", 
                                             "Langlade", "Taylor", "Marathon", "Clark")
  data[indices_northCentral, "area"] <- "North Central"
  
  indices_northEast <- data$County %in% c("Florence", "Forest", "Marinette", "Oconto", 
                                          "Menominee", "Shawano", "Door", "Kewaunee")
  data[indices_northEast, "area"] <- "North East"
  
  ## Central Districts
  indices_westCentral <- data$County %in% c("Pepin", "Jackson", "Buffalo", "Trempealeau", 
                                            "La Crosse", "Monroe", "Eau Claire", "Dunn", "Pierce", "St. Croix")
  
  data[indices_westCentral, "area"] <- "West Central"
  
  indices_central <- data$County %in% c("Wood", "Portage", "Waupaca", "Juneau", 
                                        "Adams", "Waushara", "Marquette", "Green Lake")
  data[indices_central, "area"] <- "Central"
  
  indices_eastCentral <- data$County %in% c("Outagamie", "Winnebago", "Door", "Fond du Lac", 
                                            "Brown", "Calumet","Sheboygan","Manitowoc","Kewaunee")
  
  data[indices_eastCentral, "area"] <- "East Central"
  
  ## Southern Districts
  indices_southWest <- data$County %in% c("Vernon", "Crawford", "Grant", "Richland", 
                                          "Sauk", "Iowa", "Lafayette")
  data[indices_southWest, "area"] <- "South West"
  
  indices_southCentral <- data$County %in% c("Columbia", "Dodge", "Dane", "Jefferson", 
                                             "Green", "Rock")
  data[indices_southCentral, "area"] <- "South Central"
  
  indices_southEast <- data$County %in% c("Washington", "Waukesha", "Walworth", "Ozaukee", 
                                          "Milwaukee", "Racine", "Kenosha")
  data[indices_southEast, "area"] <- "South East"
  
  
  data$logWellDepth = log(data$WellDepth)
  data$logconcentration_plus_median = log(data$concentration_plus_median)
  if(!zero_inflated){
    data <- data[data$concentration_plus_median>0.5,]
  }
  return(data)
}

permute_data = function(item,index){
  if (is.matrix(item)) {
    # If it's a matrix, subset the specified rows
    if(dim(item)[2]>1){
      item[index, , drop = FALSE] # Use drop=FALSE to prevent collapsing to vector if only one row/column remains  
    }
    else {
      # print('not matrix')
      item[index]
    }
    
  } else {
    # print('not matrix')
    # If it's NOT a matrix (e.g., a vector, data frame, etc.), keep it as is
    item[index]
  }
}


reorder_data = function(data, order = 'coordinate'){
  if(order == 'coordinate'){
    ## find the new order
    ord = order_coordinate(locs = as.matrix(data[,c('coord_x','coord_y')]))
    ## reorder all the elements in data
    ## lapply returns a list, we need to transform back to data.frame
    data = as.data.frame(lapply(data,permute_data,index = ord))
    return(data)
  } else if (order == 'maxmin'){
    ## find the new order
    ord = order_maxmin(locs = as.matrix(data[,c('coord_x','coord_y')]))
    ## reorder all the elements in data
    ## lapply returns a list, we need to transform back to data.frame
    data = as.data.frame(lapply(data,permute_data,index = ord))
    return(data)
  }
}

precision_column_calculation = function(col_index, Linv, NNarray){
  e = ifelse(1:dim(Linv)[1] == col_index, 1, 0)
  precision_column = Linv_t_mult(Linv = Linv,z = Linv_mult(Linv = Linv,z = e,NNarray = NNarray),NNarray = NNarray)
  return(precision_column)
}


# the data has to be ordered before 
# y_obs is the residuals of fitted outcome that is used to do kriging
leave_one_out_kriging = function(locs, y_obs, gp_model, gp_params, order = c("coordinate", "maxmin")){
  
  n = nrow(locs)
  locs = as.matrix(locs)
  
  ## re-order data
  order = match.arg(order)
  if(order == "coordinate"){
    ord = order_coordinate(locs = locs)  
  } else if(order == "maxmin"){
    ord = order_maxmin(locs = locs)
  }
  locs = permute_data(locs, ord)
  y_obs = permute_data(y_obs, ord)
  
  ## find the nearest neighbors
  NNarray = find_ordered_nn(locs = locs, m=30)
  ## calculate Linv
  Linv = vecchia_Linv(covparms = gp_params, covfun_name = gp_model, locs = locs, NNarray)
  
  # Then, calculate E[U_i|U_{-i}] for every i.
  y_pred = rep(NA,n)
  # should not include the position itself
  for(i in 1:n){
    if(i%%500==0)print(i)
    col_index = i
    precision_column = precision_column_calculation(col_index = col_index, Linv = Linv, NNarray = NNarray)
    y_pred[i] = -  sum(precision_column[ -col_index] * y_obs[- col_index ])/ precision_column[col_index]
  }
  
  # ## get full precision matrix
  # Precision = Linv %*% t(Linv)   # since Linv is triangular factor of precision
  # 
  # ## compute predictions in vectorized form
  # # diag_vec = diagonal elements P_ii
  # diag_vec = diag(Precision)
  # # numerator = (Precision %*% y) - P_ii * y_i
  # numer = Precision %*% y_obs - diag_vec * y_obs
  # y_pred = - numer / diag_vec
  
  
  # reverse to the original order
  inv_ord <- integer(length(ord))
  inv_ord[ord] <- 1:n
  y_pred <- permute_data(y_pred, inv_ord)
  
  return(y_pred)
}


# This is still a very hard problem. The main thing is there is no fast way of getting the diagonal of the precision matrix
# However, since the nitrate prediction does not need the leave-one-out kriging, we are fine with this issue now.

leave_one_out_kriging_vectorize = function(locs, y_obs, gp_model, gp_params, order = c("coordinate", "maxmin")){
  
  n = nrow(locs)
  locs = as.matrix(locs)
  
  ## re-order data
  order = match.arg(order)
  if(order == "coordinate"){
    ord = order_coordinate(locs = locs)  
  } else if(order == "maxmin"){
    ord = order_maxmin(locs = locs)
  }
  locs = permute_data(locs, ord)
  y_obs = permute_data(y_obs, ord)
  
  ## find the nearest neighbors
  NNarray = find_ordered_nn(locs = locs, m=30)
  ## calculate Linv
  Linv = vecchia_Linv(covparms = gp_params, covfun_name = gp_model, locs = locs, NNarray)
  
  # Then, calculate E[U_i|U_{-i}] for every i.
  y_pred = rep(NA,n)
  # should not include the position itself
  for(i in 1:n){
    if(i%%500==0)print(i)
    col_index = i
    precision_column = precision_column_calculation(col_index = col_index, Linv = Linv, NNarray = NNarray)
    y_pred[i] = -  sum(precision_column[ -col_index] * y_obs[- col_index ])/ precision_column[col_index]
  }
  
  ## get full precision matrix
  Lambda = Linv %*% t(Linv)   # since Linv is triangular factor of precision, this is not the correct way 
  Lambda <- Linv_t_mult(Linv = Linv,z = Linv_mult(Linv = Linv,z = seq_along(y_obs),NNarray = NNarray),NNarray = NNarray)
  
  
  # it seems that we are not able to get it by matrix multiplication, the main reason is 
  # 1. Calculate the product of Lambda and R_hat
  Lambda_y_obs <- Lambda %*% y_obs
  
  y_obs[i] - Lambda_y_obs[i]/Lambda[i,i]
  
  # 2. Extract the diagonal elements of Lambda and invert them
  # Note: diag() on a matrix returns a vector; 1/diag() is the element-wise inverse
  inv_diag_Lambda <- diag(1 / diag(Lambda))
  
  # 3. Compute the final expression
  y_pred <- y_obs - (inv_diag_Lambda %*% Lambda_y_obs)
  
  # reverse to the original order
  inv_ord <- integer(length(ord))
  inv_ord[ord] <- 1:n
  y_pred <- permute_data(y_pred, inv_ord)
  
  return(y_pred)
}
