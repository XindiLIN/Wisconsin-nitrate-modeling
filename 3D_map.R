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
    # subtitle = "Integration of Residual Kriging and Random Forest",
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

ggsave("figures/3D_map.png", width = 10, height = 8, dpi = 300)


# ── 3-D stacked-slices map ────────────────────────────────────────────────────
# Axes: longitude (X), latitude (Y), depth in metres (Z, reversed so deeper = lower)
# Colour: predicted nitrate (mg/L, log10 scale matching the 2-D plot above)
# The four depth levels become four horizontal point-clouds stacked along Z.

library(plotly)
library(htmlwidgets)

depth_labels <- c("15 m", "30 m", "60 m", "120 m")
depth_vals   <- c(15, 30, 60, 120)

# Add depth in metres and log10 nitrate to the flat data frame
all_depths_3d <- all_depths_df %>%
  mutate(
    depth_m      = round(WellDepth * feet_meters_scale, 0),
    log10_nitrate = log10(pred_original)
  )

# Build one trace per depth so each slice gets its own legend entry
fig_3d <- plot_ly()

trace_colours <- c("#0d0887", "#7e03a8", "#cc4778", "#f89540")  # plasma-ish anchors

for (i in seq_along(depth_vals)) {
  slice <- all_depths_3d %>% filter(depth_m == depth_vals[i])
  fig_3d <- fig_3d %>%
    add_trace(
      data = slice,
      type = "scatter3d", mode = "markers",
      x = ~longitude, y = ~latitude, z = ~depth_m,
      marker = list(
        size    = 2,
        opacity = 0.75,
        color   = ~log10_nitrate,
        colorscale = "Plasma",
        showscale  = (i == 1),           # show colour bar only once
        cmin = log10(min(all_depths_df$pred_original)),
        cmax = log10(max(all_depths_df$pred_original)),
        colorbar = list(
          title      = "Nitrate (mg/L)<br>log\u2081\u2080 scale",
          tickvals   = log10(c(1, 5, 10, 20, 40)),
          ticktext   = c("1", "5", "10", "20", "40"),
          thickness  = 15,
          len        = 0.6
        )
      ),
      text      = ~paste0("Depth: ", depth_m, " m<br>",
                          "Nitrate: ", round(pred_original, 2), " mg/L<br>",
                          "Lon: ", round(longitude, 3), "<br>",
                          "Lat: ", round(latitude, 3)),
      hoverinfo = "text",
      name      = depth_labels[i],
      showlegend = TRUE
    )
}

fig_3d <- fig_3d %>%
  layout(
    title = list(text = "Nitrate Concentration Predictions by Depth",
                 font = list(size = 16)),
    scene = list(
      xaxis = list(title = "Longitude"),
      yaxis = list(title = "Latitude"),
      zaxis = list(
        title    = "Depth (m)",
        tickvals = depth_vals,
        ticktext = depth_labels,
        autorange = "reversed",          # 15 m on top, 120 m at bottom
        showgrid = TRUE,
        gridcolor = "lightgrey"
      ),
      camera = list(
        eye = list(x = 1.6, y = -1.8, z = 1.2)   # default viewing angle
      ),
      aspectmode = "manual",
      aspectratio = list(x = 1.2, y = 1.5, z = 0.8)
    ),
    legend = list(title = list(text = "Depth"), x = 0.02, y = 0.95)
  )

# Save as interactive HTML
saveWidget(fig_3d, file = "figures/3D_nitrate_map.html", selfcontained = TRUE)

# Save as static PNG (requires the webshot2 package + Chrome/Chromium)
if (requireNamespace("webshot2", quietly = TRUE)) {
  webshot2::webshot("figures/3D_nitrate_map.html",
                    file   = "figures/3D_nitrate_map.png",
                    vwidth = 1200, vheight = 900, delay = 2)
}
