############################################################
# Script: 02_speed_analysis.R
#
# Author: Lucie Laporte-Devylder
# Affiliation: University of Southern Denmark
# ORCID: 0000-0003-0406-0839
# Contact: lucie.lprt@gmail.com
#
# Project: WildDrone (http://wilddrone.eu)
# Associated Manuscript: [Title of paper, DOI once available]
#
# Date: July 2025
# Version: 1.1
#
# Purpose:
#   - Compute whale swimming speed from consecutive flukeprints
#   - Compare speed estimates from RGB and TIR sensors
#   - Test agreement between sensors (bias, RMSE, ICC, Bland–Altman)
#   - Explore spacing–speed relationship
#   - Fit linear and mixed-effects models for speed prediction
#   - Perform LOOCV to assess predictive performance
#   - Apply log–log LMM parameters to predict whale speed
#     from still images (spacing → speed)
#   - Generate reference tables and plots for prediction and validation
#
# Input:
#   - 02_speed file (CSV/tab-delimited) with columns:
#       * time (datetime, ISO format)
#       * sensor (e.g. "RGB", "TIR")
#       * name (individual ID)
#       * surface_event (event ID)
#       * printID (sequential print index)
#       * east, north (UTM coordinates in meters)
#   - 01_size file with body length estimates (for mixed models only):
#       * name, body_length
#   - df_model.txt (processed dataframe for prediction step)
#
# Output:
#   - Tables:
#       * outputs/df/df_compare.txt → RGB–TIR comparison table
#       * outputs/tables/speed_prediction_comparison.csv → Predicted vs actual speed
#       * outputs/tables/speed_reference_table.csv → Reference spacing–speed mapping
#   - Plots:
#       * outputs/plots/speed_pred_vs_true.png → Predicted vs actual scatterplot
#       * (others produced interactively: Bland–Altman, spacing–speed, LOOCV)
#   - Console output:
#       * Summary statistics (bias, RMSE, ICC, correlations, LOOCV metrics)
#       * Model summaries (lm, lmm, log–log LMM)
#
# License: CC-BY 4.0
############################################################

# ---------------------- 0. Setup ---------------------- #
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(irr)
library(lmtest)
library(merTools)
library(purrr)
library(lubridate)
library(flextable)

# Input files (relative paths)
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

# ---------------------- 1. Data Preparation ---------------------- #
df_SPEED <- df_SPEED %>%
  mutate(
    time = ymd_hms(time),
    sensor = as.factor(sensor),
    surface_event = as.factor(surface_event)
  ) %>%
  arrange(name, surface_event, sensor, printID)

# Compute speed between consecutive prints
compute_speed <- function(data) {
  data %>%
    group_by(name, surface_event, sensor) %>%
    mutate(
      lead_east  = lead(east),
      lead_north = lead(north),
      lead_time  = lead(time),
      distance_m = sqrt((lead_east - east)^2 + (lead_north - north)^2),
      delta_t_sec = as.numeric(difftime(lead_time, time, units = "secs")),
      speed_m_per_s = distance_m / delta_t_sec
    ) %>%
    ungroup()
}

df_SPEED <- compute_speed(df_SPEED)

# Summary per event
df_summary <- df_SPEED %>%
  group_by(sensor, name, surface_event) %>%
  summarise(
    mean_speed_m_s  = mean(speed_m_per_s, na.rm = TRUE),
    max_speed_m_s   = max(speed_m_per_s, na.rm = TRUE),
    total_distance_m = sum(distance_m, na.rm = TRUE),
    duration_s      = sum(delta_t_sec, na.rm = TRUE),
    n_prints        = sum(!is.na(speed_m_per_s)),
    .groups = "drop"
  )

# ---------------------- 2. RGB–TIR Speed Comparison ---------------------- #
df_mean_speed <- df_SPEED %>%
  group_by(name, surface_event, sensor) %>%
  summarise(mean_speed = mean(speed_m_per_s, na.rm = TRUE), .groups = "drop")

df_compare <- df_mean_speed %>%
  pivot_wider(names_from = sensor, values_from = mean_speed, names_prefix = "speed_") %>%
  mutate(
    diff_mps        = speed_RGB - speed_TIR,
    diff_percent_abs = 100 * abs(diff_mps) / ((speed_RGB + speed_TIR)/2),
    within_tolerance = abs(diff_mps) <= 1 | diff_percent_abs <= 20,
    mean_speed       = (speed_RGB + speed_TIR)/2
  )

write.table(df_compare, file.path(OUTPUT_DF, "df_compare.txt"), sep="\t", row.names=FALSE, quote=FALSE)

mean_bias   <- mean(df_compare$diff_mps, na.rm = TRUE)
rmse        <- sqrt(mean(df_compare$diff_mps^2, na.rm = TRUE))
# Agreement within ±0.5 m/s and ±1 m/s
agreement05 <- mean(abs(df_compare$diff_mps) <= 0.5, na.rm = TRUE) * 100
agreement1  <- mean(abs(df_compare$diff_mps) <= 1, na.rm = TRUE) * 100
cat("Agreement ±0.5 m/s:", round(agreement05,1), "%\n",
    "Agreement ±1 m/s:", round(agreement1,1), "%\n")
t_test <- t.test(df_compare$speed_TIR, df_compare$speed_RGB, paired = TRUE)
icc_val <- irr::icc(df_compare[,c("speed_RGB","speed_TIR")], model="twoway", type="agreement", unit="single")

cat("Mean bias:", mean_bias, "\nRMSE:", rmse, "\nICC:", icc_val$value, 
    "\nAgreement ±0.5 m/s:", agreement05, "\nAgreement ±1 m/s:", agreement1, 
    "\nPaired t-test p-value:", t_test$p.value, "\n")

# Bland–Altman plot
bias <- mean_bias
sd_diff <- sd(df_compare$diff_mps, na.rm=TRUE)
loa_upper <- bias + 1.96*sd_diff
loa_lower <- bias - 1.96*sd_diff

p_ba <- ggplot(df_compare, aes(x=mean_speed, y=diff_mps)) +
  geom_point(alpha=0.7) +
  geom_hline(yintercept = bias, color="blue") +
  geom_hline(yintercept = loa_upper, linetype="dashed", color="red") +
  geom_hline(yintercept = loa_lower, linetype="dashed", color="red") +
  labs(x="Mean speed (m/s)", y="Difference (RGB-TIR)", title="Bland–Altman: RGB vs TIR speeds")



# ---------------------- 3. Spacing–Speed Analysis (TIR only) ---------------------- #
df_TIR <- df_SPEED %>% filter(sensor=="TIR")

df_summary_TIR <- df_TIR %>%
  group_by(name, surface_event) %>%
  summarise(
    mean_spacing = mean(distance_m, na.rm=TRUE),
    mean_speed   = mean(speed_m_per_s, na.rm=TRUE),
    n_obs        = n(),
    .groups="drop"
  ) %>%
  mutate(swim_mode = case_when(
    mean_speed < 0.7 ~ "Resting / milling",
    mean_speed < 4   ~ "Steady travel",
    TRUE             ~ "High-speed / pursuit"
  ))
# Remove rare events
df_summary_TIR <- df_summary_TIR %>% filter(swim_mode != "High-speed / pursuit")

# Correlations
pearson_cor <- cor(df_summary_TIR$mean_spacing, df_summary_TIR$mean_speed, use="complete.obs")
spearman_cor <- cor(df_summary_TIR$mean_spacing, df_summary_TIR$mean_speed, method="spearman", use="complete.obs")
cat("Pearson correlation:", pearson_cor, "\nSpearman correlation:", spearman_cor, "\n")

p_cor <- ggplot(df_summary_TIR, aes(x = mean_spacing, y = mean_speed)) +
  geom_point(size = 2, alpha = 0.8, color = "#1f78b4") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    x = "Mean flukeprint centroid spacing (m)",
    y = "Mean speed (m/s)",
    title = "Mean flukeprint spacing vs swimming speed (TIR)"
  ) +
  theme_minimal()


# ---------------------- 4. Mixed Models ---------------------- #
if(!is.null(df_SIZE)) {
  df_summary_TIR2 <- df_summary_TIR %>%
    left_join(df_SIZE %>% group_by(name) %>% summarise(body_length=mean(body_length, na.rm=TRUE)), by = "name")
} else {
  df_summary_TIR2 <- df_summary_TIR
}

df_summary_TIR2 <- df_summary_TIR2 %>%
  mutate(
    log_spacing = log(mean_spacing),
    log_speed   = log(mean_speed),
    log_body_length = if("body_length" %in% colnames(.)) log(body_length) else NA
  )

# LM & LMM
model_lm  <- lm(mean_speed ~ mean_spacing + body_length, data = df_summary_TIR2)
dwtest(model_lm)     #check residual autocorrelation

model_lmm <- lmer(mean_speed ~ mean_spacing + body_length + (1 | name), data = df_summary_TIR2, REML = TRUE)
summary(model_lmm)
fixef(model_lmm)

# ---- Log-transform for heteroskedasticity ----
df_summary_TIR2 <- df_summary_TIR2 %>%
  mutate(log_spacing = log(mean_spacing), log_speed = log(mean_speed), log_body_length = log(body_length))

model_loglog_lmm <- lmer(log_speed ~ log_spacing + (1 | name), data = df_summary_TIR2)
model_loglog_lmm_bodylength <- lmer(log_speed ~ log_spacing + log_body_length + (1 | name), data = df_summary_TIR2)

summary(model_loglog_lmm)  #LMM ID
summary(model_loglog_lmm_bodylength) #LMM ID + Body length


# ---------------------- 5. LOOCV ---------------------- #
whales <- unique(df_summary_TIR2$name)
cv_results <- data.frame()

for(w in whales) {
  train <- df_summary_TIR2 %>% filter(name != w)
  test  <- df_summary_TIR2 %>% filter(name == w)
  mod <- lmer(log_speed ~ log_spacing + (1|name), data=train)
  preds <- predictInterval(mod, newdata=test, level=0.95, n.sims=1000, include.resid.var=TRUE)
  test$pred_speed <- exp(preds$fit)
  test$pred_lower <- exp(preds$lwr)
  test$pred_upper <- exp(preds$upr)
  cv_results <- rbind(cv_results, test)
}

rmse_cv <- sqrt(mean((cv_results$mean_speed - cv_results$pred_speed)^2))
r2_cv <- cor(cv_results$mean_speed, cv_results$pred_speed)^2
mape_cv <- mean(abs(cv_results$mean_speed - cv_results$pred_speed)/cv_results$mean_speed)*100
coverage_cv <- mean(cv_results$mean_speed >= cv_results$pred_lower & cv_results$mean_speed <= cv_results$pred_upper)*100
calib_cv <- summary(lm(mean_speed ~ pred_speed, data=cv_results))
cat("LOOCV RMSE:", rmse_cv, "R2:", r2_cv, "MAPE(%):", mape_cv, "Coverage 95% PI:", coverage_cv, "\n")

# Scatter plot: predicted vs observed
p_pred1 <- ggplot(cv_results, aes(x = mean_speed, y = pred_speed)) +
  geom_point(alpha = 0.6, size = 2, color = "#1f78b4") +
  geom_errorbar(aes(ymin = pred_lower, ymax = pred_upper), alpha = 0.2, width = 0.05, color = "#1f78b4") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Observed speed (m/s)", y = "Predicted speed (m/s)",
       title = "LOOCV predictions from log–log LMM",
       subtitle = "Blue points = predicted speed, error bars = 95% PI, red dashed = 1:1") +
  theme_minimal()


# ---------------------- 6. Speed Prediction from TIR Spacing ---------------------- #

# Define fitted log–log LMM parameters (from best-fit model):
# model_loglog_lmm <- lmer(log_speed ~ log_spacing + (1 | name), data = df_summary_TIR2)

# Fixed effects
fixed_eff <- fixef(model_loglog_lmm)         # Named vector: (Intercept) and log_spacing
b0 <- fixed_eff["(Intercept)"]  #intercept
b1 <- fixed_eff["log_spacing"]  #slope
# Standard errors
se_eff <- sqrt(diag(vcov(model_loglog_lmm)))  # Variance-covariance matrix diagonal
se0 <- se_eff["(Intercept)"]    # SE of intercept
se1 <- se_eff["log_spacing"]    # SE of slope
# Correlation of fixed effects
corr_mat <- cov2cor(vcov(model_loglog_lmm))
corr01 <- corr_mat["(Intercept)", "log_spacing"]  # correlation of fixed effects
cov01 <- corr01 * se0 * se1                       # covariance

# Random effect variance
rand_var <- as.data.frame(VarCorr(model_loglog_lmm))
sigma_rand2 <- rand_var$vcov[rand_var$grp == "name"]    # random intercept variance (from "name")
sigma_res2  <- attr(VarCorr(model_loglog_lmm), "sc")^2  # residual variance

# Check
b0; b1; se0; se1; corr01; cov01; sigma_rand2; sigma_res2


# Prediction function based on spacing
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

# Apply predictions to dataset (TIR only)
df_spacing <- df_SPEED %>% filter(sensor=="TIR") %>%
  arrange(name, surface_event, printID) %>%
  group_by(name, surface_event) %>%
  mutate(spacing_m = sqrt((east - lag(east))^2 + (north - lag(north))^2)) %>%
  ungroup()

predictions <- df_spacing %>%
  filter(!is.na(spacing_m)) %>%
  rowwise() %>%
  mutate(pred = list(predict_speed_from_spacing(spacing_m))) %>%
  unnest(pred)

# Compute mean true speed per whale/event (from RGB)
df_speed <- df_SPEED %>% filter(sensor=="RGB") %>%
  arrange(name, surface_event, time) %>%
  group_by(name, surface_event) %>%
  mutate(
    dt = as.numeric(difftime(time, lag(time), units="secs")),
    dist_m = sqrt((east - lag(east))^2 + (north - lag(north))^2),
    true_speed = dist_m / dt
  ) %>%
  ungroup()

true_speed_summary <- df_speed %>%
  group_by(name, surface_event) %>%
  summarise(mean_true_speed = mean(true_speed, na.rm=TRUE), .groups="drop")

# Join predictions with true speed
df_compare_pred <- predictions %>%
  left_join(true_speed_summary, by=c("name","surface_event")) %>%
  mutate(
    within_PI = (mean_true_speed >= PI95_lower & mean_true_speed <= PI95_upper),
    error = mean_true_speed - mean_speed
  )

# Remove one problematic events (camera/georef issues)
exclude_events <- tribble(
  ~name, ~surface_event,
  "20240811001A", 1)

for(i in 1:nrow(exclude_events)){
  if(is.na(exclude_events$surface_event[i])){
    df_compare_pred <- df_compare_pred %>% filter(name != exclude_events$name[i])
  } else {
    df_compare_pred <- df_compare_pred %>%
      filter(!(name == exclude_events$name[i] & surface_event == exclude_events$surface_event[i]))
  }
}

# Evaluate prediction performance
rmse_pred <- sqrt(mean((df_compare_pred$mean_speed - df_compare_pred$mean_true_speed)^2))
r2_pred   <- cor(df_compare_pred$mean_speed, df_compare_pred$mean_true_speed)^2
mape_pred <- mean(abs(df_compare_pred$mean_speed - df_compare_pred$mean_true_speed)/df_compare_pred$mean_speed)*100

cat("Prediction RMSE:", rmse_pred, "\nR2:", r2_pred, "\nMAPE(%):", mape_pred, "\n")

# Create comparison table
comparison_table <- df_compare_pred %>%
  select(name, surface_event, spacing_m, mean_true_speed, median_speed, mean_speed, PI95_lower, PI95_upper, within_PI, error) %>%
  mutate(across(where(is.numeric), ~ round(.x,3)))

# 1:1 plot
p_pred2 <- ggplot(comparison_table, aes(x=mean_true_speed, y=mean_speed)) +
  geom_point(color="#1f78b4", size=2, alpha=0.8) +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  labs(x="Actual mean speed (m/s, RGB)",
       y="Predicted mean speed (m/s, TIR flukeprints)",
       title="Predicted vs Actual Whale Speed") +
  xlim(0,3) + ylim(0,3) +
  theme_minimal()


# Create reference table for spacing values
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

