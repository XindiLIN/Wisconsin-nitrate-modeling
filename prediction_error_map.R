source('functions.R')

data_split <- readRDS("results/data_split.rds")

## Prediction Error Map

val_with_residuals_log10 <- data_split$data_test %>%
  mutate(
    resid_lm           = log10_trans(logconcentration_plus_median) - log10_trans(pred_lm),
    resid_lm_krig      = log10_trans(logconcentration_plus_median) - log10_trans(pred_lm_krig),
    resid_gam          = log10_trans(logconcentration_plus_median) - log10_trans(pred_gam),
    resid_gam_krig     = log10_trans(logconcentration_plus_median) - log10_trans(pred_gam_krig),
    resid_ranger       = log10_trans(logconcentration_plus_median) - log10_trans(pred_ranger),
    resid_ranger_krig  = log10_trans(logconcentration_plus_median) - log10_trans(pred_ranger_krig),
    resid_xgboost      = log10_trans(logconcentration_plus_median) - log10_trans(pred_xgboost),
    resid_xgboost_krig = log10_trans(logconcentration_plus_median) - log10_trans(pred_xgboost_krig),
    resid_krig         = log10_trans(logconcentration_plus_median) - log10_trans(pred_krig)
  )


# 2. Pivot to Long format for faceting
# This puts all residuals in one column and model names in another
val_long <- val_with_residuals_log10 %>%
  pivot_longer(
    cols = starts_with("resid_"),
    names_to = "Model",
    values_to = "Residual"
  ) %>%
  # Optional: Clean up the model names for the plot labels
  mutate(Model = gsub("resid_", "", Model),
         Model = factor(Model, levels = c("lm", "lm_krig", "gam", "gam_krig", 
                                          "ranger", "ranger_krig", "xgboost", 
                                          "xgboost_krig", "krig")))

# 3. Convert to a spatial (sf) object
# Replace 'Longitude' and 'Latitude' with your actual coordinate column names
# crs = 4326 is standard GPS coordinates (WGS84)
val_sf <- st_as_sf(val_long, 
                   coords = c("longitude", "latitude"), 
                   crs = 4326)

val_sf <- val_sf[val_sf$Model %in% c("lm","lm_krig", "ranger","ranger_krig"),]

model_names <- c(
  "lm"           = "Linear Regression",
  "lm_krig"      = "Linear Regression + Kriging",
  "ranger"       = "Random Forest",
  "ranger_krig"  = "Random Forest + Kriging"
)




counties <- counties(state = "WI", cb = TRUE, year = 2022) # 'cb = TRUE' for cartographic boundary (simplified) version
counties = counties[,c("NAME","geometry")]
colnames(counties) = c("County","geometry")


ggplot() +
  geom_sf(data = val_sf[val_sf$Model %in% c("lm","lm_krig", "ranger","ranger_krig"),], 
          aes(color = pmin(pmax(Residual,-1),1)), # trim it to make to look better
          # aes(color = Residual),
          size = 0.01) +
  geom_sf(data = counties, fill = NA, color = "grey80", size = 0.2) +
  scale_color_gradient2(low = "#000080", 
                        
                        mid = "#ffffbf", 
                        high = "#e31a1c", 
                        name = expression(paste("Prediction Error\n[log10(mg/L)]"))
  ) +
  facet_wrap(~Model, labeller = as_labeller(model_names)) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12))
