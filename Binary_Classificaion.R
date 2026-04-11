source('functions.R')

# 1. load predictions on test data
data_split <- readRDS("results/data_split.rds")
test_data <- data_split$data_test





# 2. create list/data.frame to store the results
all_metrics <- data.frame()
roc_list <- list()
conf_mat_list <- list()
combined_cm_data <- data.frame()

# column name of the predictions using different methods in test_data
pred_names <- c("pred_ranger_krig", "pred_ranger", "pred_lm", "pred_lm_krig")

# Desired display names (mapped 1-to-1)
display_names <- c("RF + Kriging", "RF", 
                   "LR", "LR + Kriging")
internal_names <- c("pred_ranger_krig", "pred_ranger", "pred_lm", "pred_lm_krig")
name_map <- setNames(display_names, internal_names)


# 3. calculate the binary classification metrics results given 2mg/L, 10mg/L threshold 

for (threshold in c(2,10)){
  conf_mat_list[[paste(threshold,'mg/L Threshold')]] <- list()
  roc_list[[paste(threshold,'mg/L Threshold')]] <- list()
  for (m in pred_names) {
    print(threshold)
    
    # Binary outcomes
    y_obs <- factor(ifelse(test_data$concentration_plus_median > threshold, "High", "Low"), 
                    levels = c("High", "Low"))
    y_pred <- factor(ifelse(back_trans(test_data[[m]]) > threshold, "High", "Low"), 
                     levels = c("High", "Low"))
    
    # Confusion Matrix metrics
    conf_mat <- confusionMatrix(y_pred, y_obs, positive = "High")
    
    df <- as.data.frame(conf_mat$table)
    df$Method <- name_map[m]
    df$Threshold_Level <- paste(threshold,"mg/L")
    
    combined_cm_data <- rbind(combined_cm_data, df)
    
    conf_mat_list[[paste(threshold,'mg/L Threshold')]][[m]] <- conf_mat
    
    
    # MCC and F1
    eval_df <- data.frame(truth = y_obs, estimate = y_pred)
    mcc_val <- mcc(eval_df, truth = truth, estimate = estimate)$.estimate
    f1_val  <- F1_Score(y_obs, y_pred)
    
    # ROC and AUC
    # Use the continuous predictions (test_data[[m]]) for the ROC curve
    roc_obj <- roc(test_data$concentration_plus_median > threshold, 
                   test_data[[m]], quiet = TRUE)
    auc_val <- as.numeric(auc(roc_obj))
    roc_list[[paste(threshold,'mg/L Threshold')]][[m]] <- roc_obj # Save for plotting
    
    # Combine into dataframe
    model_stats <- data.frame(
      Method = name_map[m],
      Accuracy = conf_mat$overall["Accuracy"],
      Sensitivity = conf_mat$byClass["Sensitivity"],
      Specificity = conf_mat$byClass["Specificity"],
      MCC = mcc_val,
      F1 = f1_val,
      AUC = auc_val,
      Threshold_Level = threshold
    )
    all_metrics <- rbind(all_metrics, model_stats)
  }
}


# 4. barplot for binary classification metrics
plot_data <- all_metrics %>%
  pivot_longer(cols = -c(Method,Threshold_Level), names_to = "Metric", values_to = "Score")

## 4.1 barplot for 10mg/L threshold

ggplot(plot_data[plot_data$Threshold_Level==10,], aes(x = Method, y = Score, fill = Method)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~Metric, scales = "free_y") +
  theme_minimal() +
  labs(title = "Comparative Model Performance of Binary Classification", subtitle = paste("Threshold:", round(threshold, 2), "mg/L")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_brewer(palette = "Set1")

ggsave("figures/binary_classification_barplot_10mg.png", width = 10, height = 6, dpi = 300)


## 4.2 barplot for 2mg/L threshold

ggplot(plot_data[plot_data$Threshold_Level==2,], aes(x = Method, y = Score, fill = Method)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~Metric, scales = "free_y") +
  theme_minimal() +
  labs(title = "Comparative Model Performance of Binary Classification", subtitle = paste("Threshold:", round(threshold, 2), "mg/L")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_brewer(palette = "Set1")

ggsave("figures/binary_classification_barplot_2mg.png", width = 10, height = 6, dpi = 300)




# 5. ROC curves

## 5.1 ROC curves given 2mg/L Threshold
png("figures/roc_curves_2mg.png", width = 800, height = 600, res = 150)
plot(roc_list[["2 mg/L Threshold"]][[1]],
     col = "#e41a1c",
     lwd = 2,
     legacy.axes = TRUE,     # Changes x-axis to 1-Specificity (0 to 1)
     asp = NA,               # Prevents the plot from being forced into a perfect square
     main = paste("ROC Curves at",round(threshold, 2), "mg/L", "Threshold"))

# Add the other lines as before
plot(roc_list[["2 mg/L Threshold"]][[2]], add = TRUE, col = "#377eb8", lwd = 2)
plot(roc_list[["2 mg/L Threshold"]][[3]], add = TRUE, col = "#4daf4a", lwd = 2)
plot(roc_list[["2 mg/L Threshold"]][[4]], add = TRUE, col = "#ff7f00", lwd = 2)

legend("bottomright", legend = c("Random Forest + Kriging", "Random Forest", "Linear Regression", "Linear Regression + Kriging"),
       col = c("#e41a1c", "#377eb8", "#4daf4a", "#ff7f00"), lwd = 2, cex = 0.8)
dev.off()

## 5.2 ROC curves given 10mg/L Threshold
png("figures/roc_curves_10mg.png", width = 800, height = 600, res = 150)
plot(roc_list[["10 mg/L Threshold"]][[1]],
     col = "#e41a1c",
     lwd = 2,
     legacy.axes = TRUE,     # Changes x-axis to 1-Specificity (0 to 1)
     asp = NA,               # Prevents the plot from being forced into a perfect square
     main = paste("ROC Curves at",round(threshold, 2), "mg/L", "Threshold"))

# Add the other lines as before
plot(roc_list[["10 mg/L Threshold"]][[2]], add = TRUE, col = "#377eb8", lwd = 2)
plot(roc_list[["10 mg/L Threshold"]][[3]], add = TRUE, col = "#4daf4a", lwd = 2)
plot(roc_list[["10 mg/L Threshold"]][[4]], add = TRUE, col = "#ff7f00", lwd = 2)

legend("bottomright", legend = c("Random Forest + Kriging", "Random Forest", "Linear Regression", "Linear Regression + Kriging"),
       col = c("#e41a1c", "#377eb8", "#4daf4a", "#ff7f00"), lwd = 2, cex = 0.8)
dev.off()



# 6. Confusion matrix

## 6.1 Confusion matrix 10mg/L

names(conf_mat_list[["10 mg/L Threshold"]]) <- c("Random Forest + Kriging", "Random Forest", "Linear Regression", "Linear Regression + Kriging")
all_cm_data <- data.frame()

for (m in names(conf_mat_list[["10 mg/L Threshold"]])) {
  # Convert the table to a data frame
  df <- as.data.frame(conf_mat_list[["10 mg/L Threshold"]][[m]]$table)
  df$Method <- m
  all_cm_data <- rbind(all_cm_data, df)
}

ggplot(all_cm_data, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  # Add the counts in the center of the tiles
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  # Use a color scale that highlights the "High" counts
  scale_fill_gradient(low = "#f7fbff", high = "#084594") +
  # Facet by Method to create the 2x2 or 1x4 grid
  facet_wrap(~Method) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Comparison",
    subtitle = "Threshold: 10 mg/L (EPA MCL)",
    x = "Predicted Nitrate Level",
    y = "Observed Nitrate Level",
    fill = "Count"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"), # Headers for each plot
    panel.grid = element_blank()
  )

ggsave("figures/confusion_matrix_10mg.png", width = 10, height = 6, dpi = 300)

## 6.2 Confusion matrix 2mg/L

names(conf_mat_list[["2 mg/L Threshold"]]) <- c("Random Forest + Kriging", "Random Forest", "Linear Regression", "Linear Regression + Kriging")
all_cm_data <- data.frame()

for (m in names(conf_mat_list[["2 mg/L Threshold"]])) {
  # Convert the table to a data frame
  df <- as.data.frame(conf_mat_list[["2 mg/L Threshold"]][[m]]$table)
  df$Method <- m
  all_cm_data <- rbind(all_cm_data, df)
}

ggplot(all_cm_data, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") +
  # Add the counts in the center of the tiles
  geom_text(aes(label = Freq), size = 5, fontface = "bold") +
  # Use a color scale that highlights the "High" counts
  scale_fill_gradient(low = "#f7fbff", high = "#084594") +
  # Facet by Method to create the 2x2 or 1x4 grid
  facet_wrap(~Method) +
  theme_minimal() +
  labs(
    title = "Confusion Matrix Comparison",
    subtitle = "Threshold: 2 mg/L (EPA MCL)",
    x = "Predicted Nitrate Level",
    y = "Observed Nitrate Level",
    fill = "Count"
  ) +
  theme(
    strip.text = element_text(size = 12, face = "bold"), # Headers for each plot
    panel.grid = element_blank()
  )

ggsave("figures/confusion_matrix_2mg.png", width = 10, height = 6, dpi = 300)



# # 7. Re-evaluation using model trained without <2mg/L observations (for 10mg/L threshold)
# 
# data_split_less_two <- readRDS("results/data_split_less_two.rds")
# test_data_less_two <- data_split_less_two$data_test
# 
# pred_names <- c("pred_ranger_krig", "pred_ranger", "pred_lm", "pred_lm_krig")
# display_names <- c("RF + Kriging", "RF", "LR", "LR + Kriging")
# name_map <- setNames(display_names, pred_names)
# 
# threshold_lt2 <- 10
# 
# all_metrics_lt2 <- data.frame()
# roc_list_lt2   <- list()
# conf_mat_list_lt2 <- list()
# all_cm_data_lt2 <- data.frame()
# 
# for (m in pred_names) {
#   y_obs  <- factor(ifelse(test_data_less_two$concentration_plus_median > threshold_lt2, "High", "Low"),
#                    levels = c("High", "Low"))
#   y_pred <- factor(ifelse(back_trans(test_data_less_two[[m]]) > threshold_lt2, "High", "Low"),
#                    levels = c("High", "Low"))
# 
#   conf_mat <- confusionMatrix(y_pred, y_obs, positive = "High")
#   conf_mat_list_lt2[[name_map[m]]] <- conf_mat
# 
#   df <- as.data.frame(conf_mat$table)
#   df$Method <- name_map[m]
#   all_cm_data_lt2 <- rbind(all_cm_data_lt2, df)
# 
#   eval_df <- data.frame(truth = y_obs, estimate = y_pred)
#   mcc_val <- mcc(eval_df, truth = truth, estimate = estimate)$.estimate
#   f1_val  <- F1_Score(y_obs, y_pred)
# 
#   roc_obj <- roc(test_data_less_two$concentration_plus_median > threshold_lt2,
#                  test_data_less_two[[m]], quiet = TRUE)
#   auc_val <- as.numeric(auc(roc_obj))
#   roc_list_lt2[[m]] <- roc_obj
# 
#   model_stats <- data.frame(
#     Method = name_map[m],
#     Accuracy    = conf_mat$overall["Accuracy"],
#     Sensitivity = conf_mat$byClass["Sensitivity"],
#     Specificity = conf_mat$byClass["Specificity"],
#     MCC = mcc_val,
#     F1  = f1_val,
#     AUC = auc_val
#   )
#   all_metrics_lt2 <- rbind(all_metrics_lt2, model_stats)
# }
# 
# 
# ## 7.1 Barplot of classification metrics (10mg/L, trained without <2mg/L)
# 
# plot_data_lt2 <- all_metrics_lt2 %>%
#   pivot_longer(cols = -Method, names_to = "Metric", values_to = "Score")
# 
# ggplot(plot_data_lt2, aes(x = Method, y = Score, fill = Method)) +
#   geom_col(alpha = 0.8) +
#   facet_wrap(~Metric, scales = "free_y") +
#   theme_minimal() +
#   labs(title = "Comparative Model Performance of Binary Classification",
#        subtitle = paste("Threshold:", threshold_lt2, "mg/L | Trained without <2mg/L observations")) +
#   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
#   scale_fill_brewer(palette = "Set1")
# 
# ggsave("figures/binary_classification_barplot_10mg_less_two.png", width = 10, height = 6, dpi = 300)
# 
# 
# ## 7.2 ROC curves (10mg/L, trained without <2mg/L)
# 
# png("figures/roc_curves_10mg_less_two.png", width = 800, height = 600, res = 150)
# plot(roc_list_lt2[[1]],
#      col = "#e41a1c", lwd = 2, legacy.axes = TRUE, asp = NA,
#      main = paste("ROC Curves at", threshold_lt2, "mg/L Threshold | Trained without <2mg/L"))
# plot(roc_list_lt2[[2]], add = TRUE, col = "#377eb8", lwd = 2)
# plot(roc_list_lt2[[3]], add = TRUE, col = "#4daf4a", lwd = 2)
# plot(roc_list_lt2[[4]], add = TRUE, col = "#ff7f00", lwd = 2)
# legend("bottomright",
#        legend = c("Random Forest + Kriging", "Random Forest", "Linear Regression", "Linear Regression + Kriging"),
#        col = c("#e41a1c", "#377eb8", "#4daf4a", "#ff7f00"), lwd = 2, cex = 0.8)
# dev.off()
# 
# 
# ## 7.3 Confusion matrix (10mg/L, trained without <2mg/L)
# 
# ggplot(all_cm_data_lt2, aes(x = Prediction, y = Reference, fill = Freq)) +
#   geom_tile(color = "white") +
#   geom_text(aes(label = Freq), size = 5, fontface = "bold") +
#   scale_fill_gradient(low = "#f7fbff", high = "#084594") +
#   facet_wrap(~Method) +
#   theme_minimal() +
#   labs(
#     title = "Confusion Matrix Comparison",
#     subtitle = "Threshold: 10 mg/L | Trained without <2mg/L observations",
#     x = "Predicted Nitrate Level",
#     y = "Observed Nitrate Level",
#     fill = "Count"
#   ) +
#   theme(
#     strip.text = element_text(size = 12, face = "bold"),
#     panel.grid = element_blank()
#   )
# 
# ggsave("figures/confusion_matrix_10mg_less_two.png", width = 10, height = 6, dpi = 300)
