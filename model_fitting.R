source('functions.R')

## load data

data <- load_nitrate_data(file_path = "data/data_Nitrate_with_covar.csv", zero_inflated = TRUE)


## Model Fitting on 70% Training Data & Prediction on 30% Test Data

data_split <- split_nitrate_data(data)

### Pure kriging

nitrate_fitted_gp <- GpGp::fit_model(data_split$data$logconcentration_plus_median,
                                     locs = data_split$data[,c("longitude", "latitude")],
                                     covfun_name = "matern_sphere")

pred_gp <- GpGp::predictions(fit = nitrate_fitted_gp, locs_pred = data_split$data_test[,c("longitude", "latitude")], X_pred = rep(1,nrow(data_split$data_test)))

data_split$data_test$pred_krig <- pred_gp


### Linear Regression 

nitrate_fitted_lm <- nitrate_prediction_SL(data = data_split$data, data_test = data_split$data_test, SL.library = "SL.lm")

data_split$data_test$pred_lm <- nitrate_fitted_lm$pred_test
data_split$data_test$pred_lm_krig <- nitrate_fitted_lm$pred_test + nitrate_fitted_lm$krige_values_test


### gam

nitrate_fitted_gam <- nitrate_prediction_gam(data = data_split$data, data_test = data_split$data_test)

data_split$data_test$pred_gam <- nitrate_fitted_gam$pred_test
data_split$data_test$pred_gam_krig <- nitrate_fitted_gam$pred_test + nitrate_fitted_gam$krige_values_test


### xgboost

nitrate_fitted_xgboost <- nitrate_prediction_xgboost(data = data_split$data, data_test = data_split$data_test)

data_split$data_test$pred_xgboost <- nitrate_fitted_xgboost$pred_test
data_split$data_test$pred_xgboost_krig <- nitrate_fitted_xgboost$pred_test + nitrate_fitted_xgboost$krige_values_test

### ranger

nitrate_fitted_ranger <- nitrate_prediction_ranger(data = data_split$data, data_test = data_split$data_test)

data_split$data_test$pred_ranger <- nitrate_fitted_ranger$pred_test
data_split$data_test$pred_ranger_krig <- nitrate_fitted_ranger$pred_test + nitrate_fitted_ranger$krige_values_test

### save fitting value on training set and prediction on validation set

saveRDS(data_split, "results/data_split.rds")



## Fitting on Full Data and 3D Prediction 

data <- load_nitrate_data(file_path = "data/data_Nitrate_with_covar.csv", zero_inflated = TRUE)
plss_covariates <- read.csv(file = "data/plss_covariates.csv")

nitrate_fitted_ranger <- nitrate_prediction_ranger(data = data) # using the whole dataset to train the model

depth_range <- c(min(data$logWellDepth),quantile(data$logWellDepth,0.9997))

treatment_name <- "logWellDepth"

treatment_values <- seq(depth_range[1], depth_range[2], by = 0.02)

data_expanded <- plss_covariates[rep(seq_len(nrow(plss_covariates)), each = length(treatment_values)), ]
# the data_expanded is very large, so the following two line might be very time-consuming
data_expanded[,treatment_name] <- rep(treatment_values, times = nrow(plss_covariates))


preds <- predict(nitrate_fitted_ranger$ranger_obj, data = data_expanded)
preds <- preds$predictions
pred_matrix <- matrix(preds, nrow = length(treatment_values), ncol = nrow(plss_covariates))

krige_values_PLSS_ranger <- GpGp::predictions(fit = nitrate_fitted_ranger$gpfit, 
                                              locs_pred = plss_covariates[,c("longitude", "latitude")], 
                                              X_pred = rep(1,nrow(plss_covariates)))

predictions_3D_grid <- list(pred_matrix = pred_matrix,
                            krige_values_PLSS_ranger = krige_values_PLSS_ranger,
                            log_well_depth_grid = treatment_values)

saveRDS(predictions_3D_grid,'results/predictions_3D_grid.rds')



# ## Model Fitting on 70% Training Data & Prediction on 30% Test Data Excluding <2mg/L Obs
# 
# data_less_two <- data[data$concentration_plus_median >= 2.5, ]
# data_split_less_two <- split_nitrate_data(data_less_two)
# 
# nitrate_fitted_ranger_less_two <- nitrate_prediction_ranger(
#   data = data_split_less_two$data,
#   data_test = data_split_less_two$data_test
# )
# 
# data_split_less_two$data_test$pred_ranger      <- nitrate_fitted_ranger_less_two$pred_test
# data_split_less_two$data_test$pred_ranger_krig <- nitrate_fitted_ranger_less_two$pred_test + nitrate_fitted_ranger_less_two$krige_values_test
# 
# ### linear regression (trained on original scale)
# nitrate_fitted_lm_less_two <- nitrate_prediction_SL(
#   data = data_split_less_two$data,
#   data_test = data_split_less_two$data_test,
#   SL.library = "SL.lm"
# )
# 
# data_split_less_two$data_test$pred_lm      <- nitrate_fitted_lm_less_two$pred_test
# data_split_less_two$data_test$pred_lm_krig <- nitrate_fitted_lm_less_two$pred_test + nitrate_fitted_lm_less_two$krige_values_test
# 
# saveRDS(data_split_less_two, "results/data_split_less_two.rds")
# 
