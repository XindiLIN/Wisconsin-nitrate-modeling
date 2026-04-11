source('functions.R')

data_split <- readRDS("results/data_split.rds")

## Draw Barplot of rMSE

### 1. Create a data frame of rMSE



#### transform back to the original scale
rmse_data <- data.frame(
  Method = c("LR", "LR", "GAM", "GAM", "XGBoost", "XGBoost", "RF", "RF", "Kriging"),
  Variant = c("Base only", "Base and Kriging", "Base only", "Base and Kriging", 
              "Base only", "Base and Kriging", "Base only", "Base and Kriging", "Kriging Only"),
  RMSE = c(
    Metrics::rmse(back_trans(data_split$data_test$logconcentration_plus_median), back_trans(data_split$data_test$pred_lm)), 
    Metrics::rmse(back_trans(data_split$data_test$logconcentration_plus_median), back_trans(data_split$data_test$pred_lm_krig)),
    Metrics::rmse(back_trans(data_split$data_test$logconcentration_plus_median), back_trans(data_split$data_test$pred_gam)),
    Metrics::rmse(back_trans(data_split$data_test$logconcentration_plus_median), back_trans(data_split$data_test$pred_gam_krig)),
    Metrics::rmse(back_trans(data_split$data_test$logconcentration_plus_median), back_trans(data_split$data_test$pred_xgboost)),
    Metrics::rmse(back_trans(data_split$data_test$logconcentration_plus_median), back_trans(data_split$data_test$pred_xgboost_krig)),
    Metrics::rmse(back_trans(data_split$data_test$logconcentration_plus_median), back_trans(data_split$data_test$pred_ranger)),
    Metrics::rmse(back_trans(data_split$data_test$logconcentration_plus_median), back_trans(data_split$data_test$pred_ranger_krig)),
    Metrics::rmse(back_trans(data_split$data_test$logconcentration_plus_median), back_trans(data_split$data_test$pred_krig))
  )
)


#### 2. Create the Barplot
ggplot(rmse_data, aes(x = reorder(Method, -RMSE), y = RMSE, fill = Variant)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(RMSE, 3)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 4.5) +
  scale_fill_manual(values = c("Base only" = "#A6CEE3", 
                               "Kriging Only" = "#B2DF8A",
                               "Base and Kriging" = "#1F78B4" 
  )) +
  coord_cartesian(ylim = c(2.5,4.4))+
  theme_minimal() +
  labs(title = "Validation RMSE Comparison",
       x = "Model Type",
       y = "Root Mean Square Error (mg/L)",
       fill = "Method") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = c(0.98, 0.98),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "white", color = "grey80"))

ggsave("figures/rmse_barplot.png", width = 8, height = 6, dpi = 300)



