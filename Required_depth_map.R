source('functions.R')

## load predictions at 3D grid

predictions_3D_grid <- readRDS('results/predictions_3D_grid.rds')

pred_matrix <- predictions_3D_grid$pred_matrix
krige_values_PLSS_ranger <- predictions_3D_grid$krige_values_PLSS_ranger
treatment_values <- predictions_3D_grid$log_well_depth_grid

plss_covariates <- read.csv(file = "/data/plss_covariates.csv")

## incremental depth search

adjusted_matrix <- sweep(pred_matrix, 2, krige_values_PLSS_ranger, "+")



pass_condition_2mg <- adjusted_matrix < log(2.5)
first_indices_2mg <- max.col(t(pass_condition_2mg), ties.method = "first")
has_any_pass_2mg <- colSums(pass_condition_2mg) > 0
first_indices_2mg[!has_any_pass_2mg] <- nrow(pred_matrix)

pass_condition_10mg <- adjusted_matrix < log(10.5)
first_indices_10mg <- max.col(t(pass_condition_10mg), ties.method = "first")
has_any_pass_10mg <- colSums(pass_condition_10mg) > 0
first_indices_10mg[!has_any_pass_10mg] <- nrow(pred_matrix)

indirect_policy_ranger_10mg <- treatment_values[first_indices_10mg]
indirect_policy_ranger_2mg <- treatment_values[first_indices_2mg]
plss_covariates$indirect_policy_ranger_10mg <- indirect_policy_ranger_10mg
plss_covariates$indirect_policy_ranger_2mg <- indirect_policy_ranger_2mg

## draw the required depth map

counties <- counties(state = "WI", cb = TRUE, year = 2022) # 'cb = TRUE' for cartographic boundary (simplified) version
counties = counties[,c("NAME","geometry")]
colnames(counties) = c("County","geometry")


plss_long_sf <- plss_covariates %>%
  dplyr::select(longitude, latitude, indirect_policy_ranger_2mg, indirect_policy_ranger_10mg) %>%
  pivot_longer(cols = starts_with("indirect_policy"), 
               names_to = "Threshold", 
               values_to = "Depth") %>%
  mutate(
    # Apply the cap and rename for the plot titles
    Threshold = recode(Threshold, 
                       "indirect_policy_ranger_2mg" = "2mg/L Threshold", 
                       "indirect_policy_ranger_10mg" = "10mg/L Threshold")
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

meters <- c(5, 25, 100, 400)

ggplot() +
  # 1. Background layer
  geom_sf(data = counties, fill = NA, color = "grey80", size = 0.1) +
  
  # 2. Prediction points
  geom_sf(data = plss_long_sf, aes(color = Depth), size = 0.05, alpha = 0.6) +
  
  # 3. Shared Color Scale
  # we will choose 5m, 50m, 100m ,300m
  scale_color_viridis_c(
    option = "turbo", # "H" in newer ggplot is turbo
    name = "Required Well Depth\n(Meter)",
    trans = "log",
    limits = c(log(10), log(1350)),
    # breaks = log(c(10, 40, 200, 1000)),
    breaks = log(meters/0.3048),
    labels = function(x) round(exp(x)*0.3048, 0)
  ) +
  
  # 4. Faceting into 1 row, 2 columns
  facet_wrap(~Threshold, ncol = 2) +
  
  # 5. Styling
  labs(
    title = "Minimum Safe Well Depth for Nitrate Thresholds",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "plain"), # Facet Titles
    legend.position = "right",
    legend.key.width = unit(0.5, "cm"),
    panel.spacing = unit(2, "lines") # Adds space between the two maps
  )


