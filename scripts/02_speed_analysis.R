############################################################
# Script: 02_speed_analysis.R
#
# Author: Lucie Laporte-Devylder
# Affiliation: University of Southern Denmark
# ORCID: 0000-0003-0406-0839
# Contact: lucie.lprt@gmail.com
#
# Project: WildDrone (http://wilddrone.eu)
# Associated Manuscript: "In the Tracks of a Whale: Inferring Size Class, Orientation, and Swimming Speed from Thermal Flukeprints". Lucie Laporte-Devylder, Henrik Skov Midtiby, Magnus Wahlberg. DOI:
# Date: January 2026
# Version: 2.0 (revised following peer review)
#
# Purpose:
#   - Compute whale swimming speed from consecutive flukeprints
#   - Compare speed estimates from RGB and TIR sensors
#   - Test agreement between sensors (bias, RMSE, MAE, nRMSE, MAPE, ICC, Bland–Altman)
#   - Explore spacing–speed relationship
#   - Fit linear and mixed-effects models for speed prediction (log–log LMM)
#   - Perform leave-one-whale-out cross-validation (LOOCV) to assess predictive performance
#   - Apply Deming regression for calibration/bias in predictions
#   - Based on LMM log model from speed analysis, predict speed of a whale from still images, based on flukeprint spacing
#   - Generate reference tables and plots for prediction and validation
#
# Input:
#   - data files 02_speed.txt and 02_speed_stills.txt (tab-delimited) with columns:
#       * time (datetime, YYY-MM-DD HH:MM:SS.SSSSSS)
#       * sensor (e.g., "RGB", "TIR")
#       * name (whale individual ID)
#       * surface_event (event ID)
#       * printID (sequential print index)
#       * east, north (UTM coordinates in meters)
#
# Output:
#   - Plots:
#       * RGB recorded speed vs TIR flukeprint centroid distance
#       * Bland–Altman plot, spacing–speed correlation, LOOCV intervals (produced interactively)
#       * LOOCV predicted vs observed scatterplot (TIR only)
#   - Console output:
#       * Summary statistics (bias, RMSE, MAE, nRMSE, MAPE, ICC, correlations, LOOCV metrics)
#       * Model summaries (linear regression, LMM, log–log LMM, Deming regression)
#
# Usage:
#   1. Place input file in `data/`
#   2. Check INPUT_SPEED and INPUT_SIZE paths below
#   3. Run script or source("02_speed_analysis.R")
#   4. Results appear in console and `results/plots/`
#
# License: CC-BY 4.0
############################################################


# ---------------------- 0. Setup ---------------------- #
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(lme4)
library(lmerTest)
library(irr)
library(lmtest)
library(Metrics)
library(performance)
library(mcr)          # Deming regression
library(merTools)

# ---------------------- 1. Load & prepare data ---------------------- #
INPUT_SPEED <- "./data/02_speed.txt"
INPUT_SIZE  <- "./data/01_size.txt"

# Output directories
OUTPUT_DIR      <- "./outputs/"
OUTPUT_DF       <- file.path(OUTPUT_DIR, "df")
OUTPUT_TABLES   <- file.path(OUTPUT_DIR, "tables")
OUTPUT_PLOTS    <- file.path(OUTPUT_DIR, "plots")
dir.create(OUTPUT_DF, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_TABLES, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_PLOTS, recursive = TRUE, showWarnings = FALSE)

# Load datasets
df_SPEED <- read.delim(INPUT_SPEED)
df_SIZE  <- if (file.exists(INPUT_SIZE)) read.csv(INPUT_SIZE) else NULL

# Compute speed
df_SPEED <- df_SPEED %>%
  mutate(
    time = ymd_hms(time),
    sensor = as.factor(sensor),
    surface_event = as.factor(surface_event)
  ) %>%
  arrange(name, surface_event, sensor, printID)

# Function: compute speed between consecutive positions ----
compute_speed <- function(data) {
  data %>%
    group_by(name, surface_event, sensor) %>%
    mutate(
      lead_east   = lead(east),
      lead_north  = lead(north),
      lead_time   = lead(time),
      distance_m  = sqrt((lead_east - east)^2 + (lead_north - north)^2),
      delta_t_sec = as.numeric(difftime(lead_time, time, units = "secs")),
      speed_m_s   = distance_m / delta_t_sec
    ) %>%
    ungroup()
}

df <- compute_speed(df_SPEED)

# Event-level mean speed per sensor
df_event_speed <- df %>%
  group_by(name, surface_event, sensor) %>%
  summarise(
    mean_speed = mean(speed_m_s, na.rm = TRUE),
    n_obs      = sum(!is.na(speed_m_s)),
    .groups = "drop"
  )


# ---------------------- 2. RGB–TIR Agreement Analysis

df_mean_speed <- df %>%
  group_by(name, surface_event, sensor) %>%
  summarise(mean_speed = mean(speed_m_s, na.rm = TRUE), .groups = "drop")

df_mean_speed <- df_mean_speed %>%
  mutate(swim_mode = case_when(
    mean_speed < 0.7 ~ "Resting / milling",
    mean_speed < 4   ~ "Steady travel",
    TRUE             ~ "High-speed / pursuit"
  ))
df_mean_speed <- df_mean_speed %>% filter(swim_mode != "High-speed / pursuit")
df_mean_speed <- df_mean_speed %>% filter(swim_mode != "Resting / milling")

df_compare <- df_mean_speed %>%
  pivot_wider(names_from = sensor, values_from = mean_speed, names_prefix = "speed_") %>%
  mutate(
    diff_mps        = speed_RGB - speed_TIR,
    diff_percent_abs = 100 * abs(diff_mps) / ((speed_RGB + speed_TIR)/2),
    within_tolerance = abs(diff_mps) <= 1 | diff_percent_abs <= 20,
    mean_speed       = (speed_RGB + speed_TIR)/2,
    abs_error  = abs(diff_mps),
    rel_error  = abs(diff_mps) / speed_RGB
  )


# ---- Agreement metrics ----
# Compute bias (mean difference) and standard deviation, ignoring NAs
valid_idx <- complete.cases(df_compare$speed_RGB, df_compare$speed_TIR, df_compare$rel_error)

bias    <- mean(df_compare$diff_mps[valid_idx], na.rm = TRUE)
sd_diff <- sd(df_compare$diff_mps[valid_idx], na.rm = TRUE)

loa_upper <- bias + 1.96 * sd_diff
loa_lower <- bias - 1.96 * sd_diff

rmse_val  <- sqrt(mean((df_compare$speed_RGB[valid_idx] - df_compare$speed_TIR[valid_idx])^2))
mae_val   <- mean(abs(df_compare$speed_RGB[valid_idx] - df_compare$speed_TIR[valid_idx]))  # robust MAE
nrmse_val <- rmse_val / mean(df_compare$speed_RGB[valid_idx])
mape_val  <- mean(df_compare$rel_error[valid_idx]) * 100
mdape_val <- median(df_compare$rel_error[valid_idx]) * 100


list(
  Bias = bias,
  RMSE = rmse_val,
  MAE = mae_val,
  nRMSE = nrmse_val,
  MAPE_percent = mape_val,
  MAPE_median = mdape_val
)

within_05 <- mean(abs(df_compare$diff_mps) <= 0.5, na.rm = TRUE) * 100
within_1  <- mean(abs(df_compare$diff_mps) <= 1, na.rm = TRUE) * 100

within_05
within_1


# ---- Paired t-test ----
t_test <- t.test(df_compare$speed_TIR, df_compare$speed_RGB, paired = TRUE)
t_test

# ---- ICC ----
icc_val <- irr::icc(
  df_compare[, c("speed_RGB", "speed_TIR")],
  model = "twoway",
  type  = "agreement",
  unit  = "single"
)
icc_val

# ---- Bias vs speed (proportional bias) ----
lm_bias <- lm(diff_mps ~ mean_speed, data = df_compare)
summary(lm_bias)


# ---------------------- 3. Spacing–Speed Relationship (TIR)
df_TIR <- df %>%
  filter(sensor == "TIR")

df_spacing_event <- df_TIR %>%
  group_by(name, surface_event) %>%
  summarise(
    mean_spacing = mean(distance_m, na.rm = TRUE),
    mean_speed   = mean(speed_m_s, na.rm = TRUE),
    n_obs        = sum(!is.na(distance_m)),
    .groups = "drop"
  ) %>%
  mutate(swim_mode = case_when(
    mean_speed < 0.7 ~ "Resting / milling",
    mean_speed < 4   ~ "Steady travel",
    TRUE             ~ "High-speed / pursuit"
  ))

# Remove rare events
df_spacing_event <- df_spacing_event %>% filter(swim_mode != "High-speed / pursuit")
df_spacing_event <- df_spacing_event %>% filter(swim_mode != "Resting / milling")


# ---- Correlations ----
cor_pearson  <- cor(df_spacing_event$mean_spacing,
                    df_spacing_event$mean_speed,
                    use = "complete.obs")

cor_spearman <- cor(df_spacing_event$mean_spacing,
                    df_spacing_event$mean_speed,
                    method = "spearman",
                    use = "complete.obs")

cor_pearson
cor_spearman


# ---------------------- 4. Mixed-Effects Models (log–log)
df_lmm <- df_spacing_event %>%
  filter(mean_spacing > 0, mean_speed > 0) %>%
  mutate(
    log_spacing = log(mean_spacing),
    log_speed   = log(mean_speed)
  )

# ---- Base log–log LMM ----
mod_loglog <- lmer(
  log_speed ~ log_spacing + (1 | name),
  data = df_lmm,
  REML = TRUE
)

summary(mod_loglog)

# ---- R² ----
r2_vals <- performance::r2_nakagawa(mod_loglog)
r2_vals


# ---------------------- 5. Leave-One-Whale-Out CV
whales <- unique(df_lmm$name)
cv_results <- data.frame()

for (w in whales) {
  
  train <- df_lmm %>% filter(name != w)
  test  <- df_lmm %>% filter(name == w)
  
  m <- lmer(log_speed ~ log_spacing + (1 | name), data = train)
  
  preds <- predictInterval(
    m,
    newdata = test,
    level = 0.95,
    n.sims = 1000,
    include.resid.var = TRUE
  )
  
  test$pred_speed <- exp(preds$fit)
  test$lwr        <- exp(preds$lwr)
  test$upr        <- exp(preds$upr)
  
  cv_results <- rbind(cv_results, test)
}

# ---- CV metrics ----
valid_idx <- complete.cases(cv_results$mean_speed, cv_results$pred_speed)
rmse_cv <- sqrt(mean((cv_results$mean_speed[valid_idx] - 
                        cv_results$pred_speed[valid_idx])^2))
mape_cv <- mean(abs(cv_results$mean_speed - cv_results$pred_speed) /
                  cv_results$mean_speed) * 100
mape_med_cv <- median(abs(cv_results$mean_speed - cv_results$pred_speed) /
                  cv_results$mean_speed) * 100
coverage <- mean(cv_results$mean_speed >= cv_results$lwr &
                   cv_results$mean_speed <= cv_results$upr) * 100

list(
  RMSE = rmse_cv,
  MAPE_percent = mape_cv,
  MAPE_median = mape_med_cv,
  Coverage_95PI = coverage
)


# ---------------------- 6. Calibration / Bias in Predictions (Deming)
deming <- mcreg(
  x = cv_results$mean_speed,
  y = cv_results$pred_speed,
  method.reg = "Deming"
)

summary(deming)


# ---------------------- 7. Predict whale speed from flukeprint spacing on still images
# Using log-log LMM parameters

# ---- Load data
df_pred <- read.csv("./data/02_speed_stills.txt", sep="\t")

# Make sure time is parsed properly
df_pred$time <- ymd_hms(df_pred$time)

# ---- Compute spacing between consecutive prints (for each whale, same event)
df_spacing <- df_pred %>%
  filter(sensor == "TIR") %>%  #applied on spacing between prints only
  arrange(name, event, print_ID) %>%
  group_by(name, event) %>%
  mutate(
    spacing_m = sqrt((east - lag(east))^2 + (north - lag(north))^2)
  ) %>%
  ungroup()

# ---- Compute actual speed from RGB (for each whale, same event)
df_speed <- df_pred %>%
  filter(sensor == "RGB") %>%
  arrange(name, event, time) %>%
  group_by(name, event) %>%
  mutate(
    dt = as.numeric(difftime(time, lag(time), units = "secs")),
    dist_m = sqrt((east - lag(east))^2 + (north - lag(north))^2),
    true_speed = dist_m / dt   # m/s
  ) %>%
  ungroup()

# ---- Define fitted log–log LMM params
# From log-log LMM model summary
b0 <- -0.77542        # intercept
b1 <-  0.33300        # slope
se0 <- 0.18271
se1 <- 0.05482
corr01 <- -0.970
cov01 <- corr01 * se0 * se1

sigma_rand2 <- 0.02087  # random intercept variance
sigma_res2  <- 0.07536  # residual variance

# ----  Prediction function
predict_speed_from_spacing <- function(spacing_m, include_random = TRUE){
  if(is.na(spacing_m) | spacing_m <= 0) return(NA)
  
  l <- log(spacing_m)
  mu_log <- b0 + b1 * l
  
  # Variance propagation
  var_fixed <- se0^2 + (l^2)*se1^2 + 2*l*cov01
  var_pred <- var_fixed + sigma_res2
  if(include_random) var_pred <- var_pred + sigma_rand2
  
  se_pred <- sqrt(var_pred)
  
  # 95% prediction interval in log space
  lower_log <- mu_log - 1.96*se_pred
  upper_log <- mu_log + 1.96*se_pred
  
  # Back-transform
  median_speed <- exp(mu_log)
  mean_speed   <- exp(mu_log + 0.5*sigma_res2)  # bias-corrected
  lower_speed  <- exp(lower_log)
  upper_speed  <- exp(upper_log)
  
  tibble(
    spacing_m_pred = spacing_m,
    median_speed = median_speed,
    mean_speed = mean_speed,
    PI95_lower = lower_speed,
    PI95_upper = upper_speed
  )
}

# ----  Apply predictions to dataset (TIR only)
predictions <- df_spacing %>%
  filter(!is.na(spacing_m)) %>%
  rowwise() %>%
  mutate(pred = list(predict_speed_from_spacing(spacing_m))) %>%
  unnest(pred)


# ---- Compute mean true speed per whale/event (from RGB)
true_speed_summary <- df_speed %>%
  group_by(name, event) %>%
  summarise(mean_true_speed = mean(true_speed, na.rm = TRUE),
            .groups = "drop")

# ----  Join predictions (TIR) with true speed (RGB)
df_compare_pred <- predictions %>%
  left_join(true_speed_summary, by = c("name", "event")) %>%
  mutate(
    within_PI = (mean_true_speed >= PI95_lower & mean_true_speed <= PI95_upper),
    error = mean_true_speed - mean_speed
  )

rmse <- sqrt(mean((df_compare_pred$mean_speed - df_compare_pred$mean_true_speed)^2))
r2 <- cor(df_compare_pred$mean_speed, df_compare_pred$mean_true_speed)^2
mape <- mean(abs(df_compare_pred$mean_speed - df_compare_pred$mean_true_speed) / df_compare_pred$mean_speed) * 100

rmse; r2; mape

# ----  Create comparison table
comparison_table <- dplyr::select(df_compare_pred,
                                  name, event, spacing_m, mean_true_speed,
                                  median_speed, mean_speed, PI95_lower, PI95_upper,
                                  within_PI, error
) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

comparison_table


# ----  Create reference table for spacing values
spacing_values <- c(1, 5, 10, 20, 30, 40, 50, 100)  # meters

reference_table <- purrr::map_dfr(spacing_values, predict_speed_from_spacing) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  rename(
    `Spacing (m)` = spacing_m_pred,
    `Median speed (m/s)` = median_speed,
    `Bias-corrected mean (m/s)` = mean_speed,
    `95% PI lower (m/s)` = PI95_lower,
    `95% PI upper (m/s)` = PI95_upper
  )

reference_table



# ---------------------- 7. Plots ---------------------- #

# (A) Swim speed vs Flukeprint centroid distance
df_spacing_event <- df_spacing_event %>%
  group_by(swim_mode) %>%
  mutate(n_whales = n_distinct(name)) %>%
  ungroup()

Fig_speed_1 <- ggplot(df_spacing_event, aes(x = mean_spacing, y = mean_speed, color = swim_mode)) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    linetype = "solid",  # solid line
    size = 0.6           # slightly thinner
  ) +
  geom_point(size = 2.5, alpha = 0.7, color = "#052d3b") +
  labs(
    x = "Mean flukeprint centroid distance (m)",
    y = "Mean speed (m/s)",
    title = "Flukeprint spacing vs whale speed",
    subtitle = paste("N events:", nrow(df_spacing_event),
                     "across", n_distinct(df_spacing_event$name), "whales")
  ) +
  theme_minimal()


# (B) Bland–Altman plot comparing RGB vs TIR estimates
Fig_speed_2 <- ggplot(df_compare, aes(x = mean_speed, y = diff_mps)) +
  geom_point(alpha=0.7) +
  geom_hline(yintercept = bias, color="blue") +
  geom_hline(yintercept = loa_upper, linetype="dotted", color="darkgreen") +
  geom_hline(yintercept = loa_lower, linetype="dotted", color="darkgreen") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -0.5, linetype = "dashed", color = "red") +
  annotate("text", x = max(df_compare$mean_speed, na.rm = TRUE), 
           y = bias, label = paste0("Bias = ", round(bias, 2)), 
           hjust = 1.1, vjust = -0.5, color = "blue") +
  annotate("text", x = max(df_compare$mean_speed, na.rm = TRUE), 
           y = loa_upper, label = paste0("+1.96 SD = ", round(loa_upper, 2)), 
           hjust = 1.1, vjust = -0.5, color = "darkgreen") +
  annotate("text", x = max(df_compare$mean_speed, na.rm = TRUE), 
           y = loa_lower, label = paste0("-1.96 SD = ", round(loa_lower, 2)), 
           hjust = 1.1, vjust = 1.5, color = "darkgreen") +
  labs(
    x = "Mean speed (m/s) of RGB and TIR",
    y = "Difference (TIR – RGB, m/s)",
    title = "Bland–Altman plot for RGB vs TIR speed estimates",
    subtitle = paste("N events:", nrow(df_compare),
                     "across", n_distinct(df_compare$name), "whales")
  )

# (C) LOOCV Predicted vs observed mean speed (TIR videos & TIR still images)
Fig_speed_3 <- ggplot() +
  # 1:1 reference line
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  # TIR still images (grey)
  geom_point(
    data = comparison_table,
    aes(x = mean_true_speed, y = mean_speed),
    color = "grey",
    size = 2
  ) +
  geom_errorbar(
    data = comparison_table,
    aes(x = mean_true_speed, ymin = PI95_lower, ymax = PI95_upper),
    color = "grey",
    alpha = 0.5,
    width = 0.05
  ) +
  # TIR videos (black)
  geom_point(
    data = cv_results,
    aes(x = mean_speed, y = pred_speed),
    color = "black",
    size = 2
  ) +
  geom_errorbar(
    data = cv_results,
    aes(x = mean_speed, ymin = lwr, ymax = upr),
    color = "black",
    alpha = 0.3,
    width = 0.05
  ) +
  labs(
    x = "Observed mean speed (m/s)",
    y = "Predicted mean speed (m/s)",
    title = "Predicted vs observed whale swimming speed",
    subtitle = paste(
      "Black = TIR videos, Grey = TIR still images"
    )
  ) +
  theme_minimal()
