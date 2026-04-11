source('functions.R')

## load predictions at 3D grid

predictions_3D_grid <- readRDS('results/predictions_3D_grid.rds')

plss_covariates <- read.csv('data/plss_covariates.csv')

pred_matrix <- predictions_3D_grid$pred_matrix
krige_values_PLSS_ranger <- predictions_3D_grid$krige_values_PLSS_ranger
treatment_values <- predictions_3D_grid$log_well_depth_grid


depths_meters <- c(15,30,60,120)

feet_meters_scale <- 0.3048

depths_feet <- depths_meters/feet_meters_scale

# depths_feet <- c(50, 100, 200, 400)

grid_indice <- lapply(depths_feet, FUN = function(x){which.min(abs(treatment_values - feet))}, simplify = TRUE)


# 1. Define depths and combine into one master data.frame


all_depths_df <- map_dfr(depths_feet, function(d) {
  plss_covariates %>%
    mutate(
      WellDepth = d,
      logWellDepth = log(d),
      Depth_Label = paste(round(d*0.3048,0), "Meters"), # Useful for facet titles
      pred_ranger = pred_matrix[which.min(abs(exp(treatment_values) - d)),],
      krige_ranger = krige_values_PLSS_ranger
    )
})


# 2. Transform to original scale and apply the floor (pmax)
all_depths_df <- all_depths_df %>%
  mutate(
    pred_original = exp(pred_ranger + krige_ranger),
    pred_original = pmax(pred_original, 0.4274596)
  )

# 3. Convert to SF object once
all_depths_sf <- st_as_sf(all_depths_df, coords = c("longitude", "latitude"), crs = 4326)


# 4. plot log_10 legend


counties <- counties(state = "WI", cb = TRUE, year = 2022) # 'cb = TRUE' for cartographic boundary (simplified) version
counties = counties[,c("NAME","geometry")]
colnames(counties) = c("County","geometry")



ggplot() +
  # Points Layer
  geom_sf(data = all_depths_sf, aes(color =pred_original), size = 0.01) +
  # Counties Layer (drawn on top for clarity)
  geom_sf(data = counties, fill = NA, color = "black", size = 0.1) +
  # Shared Color Scale
  scale_color_viridis_c(
    option = "plasma", 
    name = "Predicted Nitrate\n(mg/L)",
    trans = "log10", 
    # trans = "log", 
    limits = c(min(all_depths_df$pred_original), max(all_depths_df$pred_original)),
    breaks = c(1, 5, 10, 20, 40),
    labels = c( "1", "5", "10", "20", "40")
  ) +
  # Create the 2x2 Grid
  facet_wrap(~reorder(Depth_Label, WellDepth), ncol = 2) +
  labs(
    title = "Nitrate Concentration Predictions by Depth",
    subtitle = "Integration of Residual Kriging and Random Forest",
    # x = "Longitude", 
    # y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "plain"),
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.key.width = unit(0.7, "cm")
  )
