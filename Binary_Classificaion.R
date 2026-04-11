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


## 4.2 barplot for 2mg/L threshold

ggplot(plot_data[plot_data$Threshold_Level==2,], aes(x = Method, y = Score, fill = Method)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~Metric, scales = "free_y") +
  theme_minimal() +
  labs(title = "Comparative Model Performance of Binary Classification", subtitle = paste("Threshold:", round(threshold, 2), "mg/L")) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_brewer(palette = "Set1")




# 5. ROC curves

## 5.1 ROC curves given 2mg/L Threshold
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

## 5.2 ROC curves given 10mg/L Threshold

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
