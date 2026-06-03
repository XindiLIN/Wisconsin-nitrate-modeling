# Wisconsin Nitrate Modeling

Predicts groundwater nitrate concentrations across Wisconsin private wells by combining machine learning models with spatial Gaussian process residual kriging. The workflow covers model training and evaluation, 3D depth-resolved prediction, and policy-relevant maps showing the minimum safe well depth to stay below EPA nitrate standards.

## Overview

Nitrate contamination in private wells is a major public health concern in agricultural regions. This project builds and compares several regression models that jointly leverage well characteristics, land use, and spatial correlation to predict nitrate levels. A key contribution is the residual kriging step: each base model's spatial residuals are fit with a Matérn covariance GP (via `GpGp`), whose predictions are added back at inference time to capture spatial autocorrelation that tabular covariates miss.

## Repository Structure

```
Wisconsin_nitrate_modeling/
├── data/
│   ├── data_Nitrate_with_covar.csv   # Well observations with covariates
│   └── plss_covariates.csv           # PLSS grid points for statewide prediction
├── functions.R                       # Core model wrappers and data utilities
├── model_fitting.R                   # Train/test split, fit all models, 3D grid prediction
├── rMSE_barplot.R                    # RMSE comparison barplot
├── Binary_Classificaion.R            # Binary classification at EPA thresholds
├── prediction_error_map.R            # Spatial prediction error maps
├── Required_depth_map.R              # Minimum safe well depth maps
├── 3D_map.R                          # Depth-slice and interactive 3D maps
├── results/                          # Saved RDS model outputs
└── figures/                          # Generated PNG figures
```

## Methods

### Models

Five approaches are benchmarked, each optionally augmented with GP residual kriging:

| Model | R implementation |
|---|---|
| Linear Regression | `SuperLearner` with `SL.lm` |
| Generalized Additive Model (GAM) | `mgcv::gam` with `s()` smooths |
| XGBoost | `xgboost` (10-fold CV for GP residuals) |
| Random Forest | `ranger` |
| Kriging only | `GpGp` on raw log-concentration |

### Covariates

`logWellDepth`, `crop_type_combine`, `drainagecl`, `precipitation`, `cafolog`, `StaticLevel`

### Target variable

`logconcentration_plus_median = log(concentration + median)`. Back-transformed via `exp(x) − 0.5`.

### Train / test split

70 / 30 stratified split on the log-concentration distribution (`caret::createDataPartition`).

## Scripts

### `model_fitting.R`

Fits all models on the training set, adds residual kriging predictions on the test set, and saves `results/data_split.rds`. Also fits a Random Forest on the full dataset and generates predictions on a dense 3D PLSS grid over a range of well depths, saved to `results/predictions_3D_grid.rds`.

### `rMSE_barplot.R`

Loads `data_split.rds`, back-transforms predictions to mg/L, and produces a grouped barplot comparing RMSE across all models (base only vs. base + kriging). Output: `figures/rmse_barplot.png`.

### `Binary_Classificaion.R`

Evaluates binary classification performance at the 2 mg/L and 10 mg/L EPA thresholds. Produces:
- Accuracy, Sensitivity, Specificity, MCC, F1, AUC barplots
- ROC curves
- Confusion matrix heatmaps

for Random Forest, Random Forest + Kriging, Linear Regression, and Linear Regression + Kriging.

### `3D_map.R`

Visualizes statewide nitrate predictions at four depths (15, 30, 60, 120 m) as faceted 2D maps and an interactive 3D HTML widget (`figures/3D_nitrate_map.html`). Color scale is log₁₀ nitrate (mg/L) using the plasma palette.

### `Required_depth_map.R`

For each PLSS grid point, finds the shallowest well depth at which the predicted nitrate falls below each EPA threshold. Maps the resulting "minimum safe well depth" statewide for both the 2 mg/L and 10 mg/L thresholds. Output: `figures/required_depth_map.png`.

### `prediction_error_map.R`

Plots spatially resolved prediction errors (log₁₀ scale) for Linear Regression, Linear Regression + Kriging, Random Forest, and Random Forest + Kriging. Output: `figures/prediction_error_map.png`.

### `functions.R`

Shared utilities:
- `load_nitrate_data()` — reads CSV, assigns regional labels, log-transforms covariates
- `split_nitrate_data()` — stratified train/test split
- `nitrate_prediction_SL/ranger/gam/xgboost/svm()` — unified model-fitting wrappers that return base predictions and GP residual kriging values
- `leave_one_out_kriging()` — LOO kriging via Vecchia approximation (Cholesky precision columns)
- `back_trans()` / `log10_trans()` — concentration scale transformations

## Dependencies

Install the following R packages before running:

```r
install.packages(c(
  "tidymodels", "tidyverse", "ranger", "SuperLearner", "sf", "GpGp",
  "Metrics", "rnaturalearth", "rnaturalearthdata", "yardstick", "xgboost",
  "glmnet", "kernlab", "e1071", "broom", "gratia", "ggeffects", "caret",
  "recipes", "pROC", "MLmetrics", "tigris", "mgcv", "WeightIt",
  "plotly", "htmlwidgets"
))
# For static 3D PNG export:
install.packages("webshot2")
```

## Reproducibility

Run the scripts in this order:

```r
source("model_fitting.R")         # Fit models; saves results/data_split.rds and results/predictions_3D_grid.rds
source("rMSE_barplot.R")          # RMSE comparison
source("Binary_Classificaion.R")  # Classification metrics
source("prediction_error_map.R")  # Spatial error maps
source("3D_map.R")                # Depth-slice maps
source("Required_depth_map.R")    # Minimum safe well depth maps
```

The raw data files (`data/data_Nitrate_with_covar.csv` and `data/plss_covariates.csv`) are stored in iCloud and may need to be downloaded locally before running.
