############################################################
# Title: 01_size_analysis.R
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
# Version: 1.0
#
# Purpose:
#   - Explore relationship between flukeprint width and whale body size
#   - Test whether flukeprint width can be used as a proxy for whale dimensions
#   - Fit linear models (with/without age class effects)
#   - Perform cross-validation
#   - Classify age class using logistic regression
#
# Input:
#   - Dataset file (CSV or tab-delimited)
#   - Must contain the following columns:
#       * name (individual ID)
#       * age_class (categorical: e.g., calf, adult, subadult)
#       * flukeprint_width (numeric, meters)
#       * body_length (numeric, meters)
#       * fluke_span (numeric, meters)
#
# Output:
#   - Model summaries (console)
#   - Cross-validation results
#   - Diagnostic and result plots
#
# Reproducibility:
#   - R version: 4.4.3
#   - Required packages: ggplot2, dplyr, caret, lme4, pROC, cowplot, reshape2
#   - Random seed set (123)
#
# Usage:
#   1. Place raw dataset into `data/` directory
#   2. Update INPUT_FILE variable in script
#   3. Run script line by line or source("01_size_analysis.R")
#   4. Outputs (plots, results) will appear in console or be saved to `results/`
#
# License: CC-BY 4.0
############################################################


# ---------------------- 0. Setup ---------------------- #
# Load required packages
library(ggplot2)
library(dplyr)
library(caret)
library(lme4)     # may be needed for glm/lmer if expanded
library(pROC)
library(cowplot)
library(reshape2)

# Reproducibility
set.seed(123)

# ---------------------- 1. Load data ---------------------- #
# Input files (user-defined, relative paths recommended)
INPUT_FILE <- "./data/01_size.txt"

# Adjust delimiter as needed ("," for CSV, "\t" for tab-delimited)
df_size <- read.csv(INPUT_FILE, stringsAsFactors = FALSE)

# Inspect structure
str(df_size)


# ---------------------- 2. Summarise per individual ---------------------- #
df_summary <- df_size %>%
  group_by(name, age_class) %>%
  summarise(
    mean_flukeprint_width = mean(flukeprint_width, na.rm = TRUE),
    mean_body_length = mean(body_length, na.rm = TRUE),
    mean_fluke_span = mean(fluke_span, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(mean_flukeprint_width),
         !is.na(mean_body_length),
         !is.na(mean_fluke_span))


# ---------------------- 3. Cross-validation setup ---------------------- #
ctrl <- trainControl(method = "cv", number = 10)


# ---------------------- 4. Linear models (no age class) ---------------------- #
model_body_simple <- lm(mean_body_length ~ mean_flukeprint_width, data = df_summary)
model_span_simple <- lm(mean_fluke_span ~ mean_flukeprint_width, data = df_summary)

summary(model_body_simple)
summary(model_span_simple)

# CV versions
cv_model_body_simple <- train(mean_body_length ~ mean_flukeprint_width,
                              data = df_summary, method = "lm", trControl = ctrl)
cv_model_span_simple <- train(mean_fluke_span ~ mean_flukeprint_width,
                              data = df_summary, method = "lm", trControl = ctrl)

print(cv_model_body_simple)
print(cv_model_span_simple)


# ---------------------- 5. Linear models (with age class) ---------------------- #
df_summary$age_class <- as.factor(df_summary$age_class)

# Keep only two classes (drop rare categories)
df_summary_binary <- df_summary %>%
  filter(age_class %in% c("calf", "adult")) %>%
  droplevels()


# Models with interaction
model_body_age <- lm(mean_body_length ~ mean_flukeprint_width * age_class,
                     data = df_summary_binary)
model_span_age <- lm(mean_fluke_span ~ mean_flukeprint_width * age_class,
                     data = df_summary_binary)

summary(model_body_age)
summary(model_span_age)

# CV models with interaction
cv_model_body_age <- train(mean_body_length ~ mean_flukeprint_width * age_class,
                           data = df_summary_binary, method = "lm", trControl = ctrl)
cv_model_span_age <- train(mean_fluke_span ~ mean_flukeprint_width * age_class,
                           data = df_summary_binary, method = "lm", trControl = ctrl)

print(cv_model_body_age)
print(cv_model_span_age)


# ---------------------- 6. Logistic regression (age class prediction) ---------------------- #
model_age_class <- train(age_class ~ mean_flukeprint_width,
                         data = df_summary_binary,
                         method = "glm",
                         family = "binomial",
                         trControl = ctrl)

print(model_age_class)

# Predicted class probabilities
pred_probs <- predict(model_age_class, newdata = df_summary_binary, type = "prob")

# Convert to class predictions (cutoff = 0.5)
pred_class <- ifelse(pred_probs$adult >= 0.5, "adult", "calf") %>%
  factor(levels = c("calf", "adult"))

# Confusion matrix
conf_mat <- confusionMatrix(pred_class, df_summary_binary$age_class)
print(conf_mat)


# ---------------------- 7. ROC analysis ---------------------- #
roc_obj <- roc(df_summary_binary$age_class,
               df_summary_binary$mean_flukeprint_width,
               levels = c("calf", "adult"))  # adult = positive class

opt <- coords(roc_obj, "best",
              ret = c("threshold", "sensitivity", "specificity"),
              best.method = "youden")
print(opt)

# Apply threshold
pred_class <- ifelse(df_summary_binary$mean_flukeprint_width > opt["threshold"], "adult", "calf")
pred_class <- factor(pred_class, levels = c("calf", "adult"))

conf_mat <- caret::confusionMatrix(pred_class, df_summary_binary$age_class)
print(conf_mat)


# ---------------------- 8. Plots ---------------------- #
# (A) Body length vs flukeprint width
p1 <- ggplot(df_summary, aes(x = mean_flukeprint_width, y = mean_body_length, color = age_class)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Body Length ~ Flukeprint Width",
       x = "Mean Flukeprint Width (m)", y = "Mean Body Length (m)") +
  theme_minimal()

# (B) Fluke span vs flukeprint width
p2 <- ggplot(df_summary, aes(x = mean_flukeprint_width, y = mean_fluke_span)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "Fluke Span ~ Flukeprint Width",
       x = "Mean Flukeprint Width", y = "Mean Fluke Span") +
  theme_minimal()

# (C, D) With age class interaction
p3 <- ggplot(df_summary_binary, aes(x = mean_flukeprint_width, y = mean_body_length, color = age_class)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, aes(linetype = age_class)) +
  labs(title = "Body Length by Age Class",
       x = "Mean Flukeprint Width", y = "Mean Body Length") +
  theme_minimal()

p4 <- ggplot(df_summary_binary, aes(x = mean_flukeprint_width, y = mean_fluke_span, color = age_class)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, aes(linetype = age_class)) +
  labs(title = "Fluke Span by Age Class",
       x = "Mean Flukeprint Width", y = "Mean Fluke Span") +
  theme_minimal()

# (E, F) CV predicted vs observed
pred_body <- predict(cv_model_body_simple, df_summary)
p5 <- ggplot(df_summary, aes(x = mean_body_length, y = pred_body)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predicted vs Observed Body Length",
       x = "Observed", y = "Predicted") +
  theme_minimal()

pred_span <- predict(cv_model_span_simple, df_summary)
p6 <- ggplot(df_summary, aes(x = mean_fluke_span, y = pred_span)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Predicted vs Observed Fluke Span",
       x = "Observed", y = "Predicted") +
  theme_minimal()

# (G) ROC curve
probs <- predict(model_age_class, df_summary_binary, type = "prob")
roc_obj <- roc(df_summary_binary$age_class, probs$adult)

roc_df <- data.frame(
  tpr = rev(roc_obj$sensitivities),
  fpr = rev(1 - roc_obj$specificities)
)

p7 <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "purple", size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "ROC Curve: Age Class Prediction",
       x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()

# (H) Confusion matrix heatmap
cm_df <- as.data.frame(conf_mat$table)
colnames(cm_df) <- c("Reference", "Prediction", "Freq")

p8 <- ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix: Age Class Prediction",
       x = "Actual Age Class", y = "Predicted Age Class") +
  theme_minimal(base_size = 14)

#Density plots showing separation between calves and adults (flukeprint width)
p9 <- ggplot(df_summary_binary, aes(x = mean_flukeprint_width, color = age_class, fill = age_class)) +
  geom_density(alpha = 0.3) +
  labs(
    title = "Density of Flukeprint width by Age Class",
    x = "Mean Flukeprint width (m)",
    y = "Density"
  ) +
  theme_minimal()


# ---------------------- 9. Appendix: Fluke span vs body length ---------------------- #
df_summary2 <- df_size %>%
  group_by(name) %>%
  summarise(
    mean_fluke_span = mean(fluke_span, na.rm = TRUE),
    mean_body_length = mean(body_length, na.rm = TRUE),
    age_class = first(age_class),
    .groups = "drop"
  ) %>%
  filter(!is.na(mean_fluke_span), !is.na(mean_body_length), !is.na(age_class))

ggplot(df_summary2, aes(x = mean_body_length, y = mean_fluke_span, color = age_class)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Fluke Span vs Body Length",
       x = "Mean Body Length (m)", y = "Mean Fluke Span (m)",
       color = "Age Class") +
  theme_minimal()

model_appendix <- lm(mean_fluke_span ~ mean_body_length, data = df_summary2)
summary(model_appendix)
