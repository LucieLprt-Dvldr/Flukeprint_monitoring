############################################################
# Title: 01_size_analysis.R
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
#   - Assess information content of flukeprint width
#   - Test separation between calves and adults
#   - Evaluate size scaling within age classes
#   - Compare scaling slopes between age classes
#   - Classify age class using flukeprint width
#
# License: CC-BY 4.0
############################################################


# ---------------------- 0. Setup ---------------------- #
library(ggplot2)
library(dplyr)
library(caret)
library(lme4)
library(pROC)
library(cowplot)
library(reshape2)

set.seed(123)


# ---------------------- 1. Load data ---------------------- #
INPUT_FILE <- "./data/01_size_revised.txt"
df_size <- read.csv(INPUT_FILE, stringsAsFactors = FALSE)
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
  filter(
    !is.na(mean_flukeprint_width),
    !is.na(mean_body_length),
    !is.na(mean_fluke_span)
  )

df_summary$age_class <- factor(df_summary$age_class)


# ---------------------- 3. Cross-validation setup ---------------------- #
ctrl <- trainControl(method = "cv", number = 10)


# ---------------------- 4. Descriptive global models ---------------------- #
# (reported descriptively only; clustering expected)
model_body_global <- lm(mean_body_length ~ mean_flukeprint_width,
                        data = df_summary)
model_span_global <- lm(mean_fluke_span ~ mean_flukeprint_width,
                        data = df_summary)

summary(model_body_global)
summary(model_span_global)


# ---------------------- 5. Age-class interaction models ---------------------- #
# Test whether scaling differs between calves and adults
model_body_interaction <- lm(
  mean_body_length ~ mean_flukeprint_width * age_class,
  data = df_summary
)

model_span_interaction <- lm(
  mean_fluke_span ~ mean_flukeprint_width * age_class,
  data = df_summary
)

anova(model_body_interaction)
anova(model_span_interaction)
summary(model_body_interaction)
summary(model_span_interaction)


# ---------------------- 6. Within age-class regressions ---------------------- #
# Explicitly test information content within size classes

model_body_adult <- lm(
  mean_body_length ~ mean_flukeprint_width,
  data = subset(df_summary, age_class == "adult")
)

model_body_calf <- lm(
  mean_body_length ~ mean_flukeprint_width,
  data = subset(df_summary, age_class == "calf")
)

model_span_adult <- lm(
  mean_fluke_span ~ mean_flukeprint_width,
  data = subset(df_summary, age_class == "adult")
)

model_span_calf <- lm(
  mean_fluke_span ~ mean_flukeprint_width,
  data = subset(df_summary, age_class == "calf")
)

summary(model_body_adult)
summary(model_body_calf)
summary(model_span_adult)
summary(model_span_calf)


# ---------------------- 7. Cross-validation (adults only) ---------------------- #
# Assess predictive performance where scaling exists

cv_body_adult <- train(
  mean_body_length ~ mean_flukeprint_width,
  data = subset(df_summary, age_class == "adult"),
  method = "lm",
  trControl = ctrl
)

cv_span_adult <- train(
  mean_fluke_span ~ mean_flukeprint_width,
  data = subset(df_summary, age_class == "adult"),
  method = "lm",
  trControl = ctrl
)

print(cv_body_adult)
print(cv_span_adult)


# ---------------------- 8. Logistic regression: age-class discrimination ---------------------- #
model_age_class <- glm(
  age_class ~ mean_flukeprint_width,
  data = df_summary,
  family = binomial
)

summary(model_age_class)


# ---------------------- 9. ROC analysis and optimal threshold ---------------------- #
roc_obj <- roc(
  response = df_summary$age_class,
  predictor = df_summary$mean_flukeprint_width,
  levels = c("calf", "adult"),
  direction = "<"
)

opt <- coords(
  roc_obj,
  "best",
  ret = c("threshold", "sensitivity", "specificity"),
  best.method = "youden"
)

print(opt)

# Classification using Youden threshold
df_summary$pred_class <- ifelse(
  df_summary$mean_flukeprint_width < opt["threshold"],
  "calf",
  "adult"
)

df_summary$pred_class <- factor(
  df_summary$pred_class,
  levels = c("calf", "adult")
)

conf_mat <- confusionMatrix(
  df_summary$pred_class,
  df_summary$age_class
)

print(conf_mat)


# ---------------------- 10. Plots ---------------------- #

# (A) Body length vs flukeprint width (age-specific slopes)
Fig_size_1 <- ggplot(df_summary, aes(
  x = mean_flukeprint_width, 
  y = mean_body_length,
  color = age_class, 
  shape = age_class, 
  linetype = age_class
)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, show.legend = FALSE, linewidth = 0.6) +
  scale_color_manual(values = c(
    "calf" = "#66c2a5",   # teal for calves
    "adult" = "#0b1d51"   # very dark blue for adults
  )) +
  scale_shape_manual(values = c(
    "calf" = 17,   # triangle for calves
    "adult" = 16   # circle for adults
  )) +
  scale_linetype_manual(values = c(
    "calf" = "dashed", 
    "adult" = "solid"
  )) +
  labs(
    title = "Body Length vs Flukeprint Width by Age Class",
    subtitle = paste("N adult = 80, N calf = 12"),
    x = "Mean Flukeprint Width (m)",
    y = "Mean Body Length (m)",
    color = "Age Class",
    shape = "Age Class",
    linetype = "Age Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )


# (B) Fluke span vs flukeprint width (age-specific slopes)
Fig_size_2 <- ggplot(df_summary, aes(
  x = mean_flukeprint_width, 
  y = mean_fluke_span,
  color = age_class, 
  shape = age_class, 
  linetype = age_class
)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, show.legend = FALSE, linewidth = 0.6) +
  scale_color_manual(values = c(
    "calf" = "#66c2a5",   # teal for calves
    "adult" = "#0b1d51"   # very dark blue for adults
  )) +
  scale_shape_manual(values = c(
    "calf" = 17,   # triangle for calves
    "adult" = 16   # circle for adults
  )) +
  scale_linetype_manual(values = c(
    "calf" = "dashed", 
    "adult" = "solid"
  )) +
  labs(
    title = "Fluke span vs Flukeprint Width by Age Class",
    x = "Mean Flukeprint Width (m)",
    y = "Mean Fluke span (m)",
    color = "Age Class",
    shape = "Age Class",
    linetype = "Age Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )


# (C) Density plot flukeprint width of adults / calves
Fig_size_3 <- ggplot(df_summary, aes(
  x = mean_flukeprint_width,
  color = age_class,
  fill = age_class,
  linetype = age_class   
)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  scale_color_manual(values = c(
    "calf" = "#66c2a5",   # teal
    "adult" = "#0b1d51"   # very dark blue
  )) +
  scale_fill_manual(values = c(
    "calf" = "#66c2a5",
    "adult" = "#0b1d51"
  )) +
  scale_linetype_manual(values = c(
    "calf" = "dashed",    # calf dashed
    "adult" = "solid"     # adult solid
  )) +
  labs(
    title = "Density of Flukeprint Width by Age Class",
    x = "Mean Flukeprint Width (m)",
    y = "Density",
    color = "Age Class",
    fill = "Age Class",
    linetype = "Age Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(size = 0.8),   # make major grid more visible
    panel.grid.minor = element_line(size = 0.4)    # make minor grid thinner
  )


# (D) ROC curve
roc_df <- data.frame(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

ROC_size <- ggplot(roc_df, aes(fpr, tpr)) +
  geom_line(size = 1) +
  geom_abline(linetype = "dashed") +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  theme_minimal()


# ---------------------- 11. Appendix: fluke span vs body length ---------------------- #
df_appendix <- df_summary %>%
  select(mean_body_length, mean_fluke_span, age_class)

model_appendix <- lm(mean_fluke_span ~ mean_body_length, data = df_appendix)
summary(model_appendix)

# (E) Fluke span vs flukeprint width (age-specific slopes)
Fig_size_4 <- ggplot(df_appendix, aes(
  x = mean_body_length,
  y = mean_fluke_span,
  color = age_class,
  shape = age_class,
  linetype = age_class
)) +
  geom_point(size = 2, alpha = 0.8) +  # slightly bigger for readability
  geom_smooth(method = "lm", se = TRUE, aes(color = age_class, linetype = age_class), size = 1) +
  scale_color_manual(values = c(
    "calf" = "#66c2a5",   # teal for calves
    "adult" = "#0b1d51"   # very dark blue for adults
  )) +
  scale_shape_manual(values = c(
    "calf" = 17,   # triangle
    "adult" = 16   # circle
  )) +
  scale_linetype_manual(values = c(
    "calf" = "dashed",
    "adult" = "solid"
  )) +
  labs(
    title = "Fluke Span vs Body Length",
    x = "Mean Body Length (m)",
    y = "Mean Fluke Span (m)",
    color = "Age Class",
    shape = "Age Class",
    linetype = "Age Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )
