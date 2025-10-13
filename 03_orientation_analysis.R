############################################################
# Script: 03_orientation_analysis.R
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
#   - Validate whale heading estimates from TIR flukeprints
#     against RGB "ground truth"
#   - Compute heading differences with circular correction
#   - Classify agreement (Excellent, Good, Acceptable, Discrepancy)
#   - Perform circular statistics (means, correlation, Rayleigh test)
#   - Summarise per-whale and global agreement
#   - Visualize agreement (scatter, histogram, polar plots)
#
# Input:
#   - 03_orientation file (CSV/tab-delimited) with columns:
#       * name (individual ID)
#       * event (surface event ID)
#       * printID (sequential print index)
#       * time (datetime, ISO format)
#       * sensor ("RGB" or "TIR")
#       * heading (°)
#
# Output:
#   - Agreement classification table
#   - Summary statistics (circular means, SDs, correlation, Rayleigh test)
#   - Per-whale and global summaries
#   - Plots: RGB vs TIR scatter, difference histogram, circular polar plot
#
# Reproducibility:
#   - R version: 4.4.3
#   - Required packages: dplyr, ggplot2, circular, CircStats, tidyverse, sp
#
# Usage:
#   1. Place input file in `data/`
#   2. Update INPUT_ORIENTATION path below
#   3. Run script or source("03_orientation_analysis.R")
#   4. Results appear in console and `results/plots/`
#
# License: [e.g., CC-BY 4.0 or MIT License]
############################################################


# ---------------------- 0. Setup ---------------------- #
library(dplyr)
library(tidyverse)
library(ggplot2)
library(circular)
library(CircStats)
library(sp)   # optional, for geographic calculations

# Input files (user-defined, relative paths recommended)
INPUT_ORIENTATION <- "./data/03_orientation.txt"

# Load dataset
df_orientation <- read.delim(INPUT_ORIENTATION, stringsAsFactors = FALSE)

# Ensure timestamps are proper POSIXct
df_orientation$time <- as.POSIXct(df_orientation$time)


# ---------------------- 1. Prepare TIR & RGB ---------------------- #
orientation_tir <- df_orientation %>% filter(sensor == "TIR")
orientation_rgb <- df_orientation %>% filter(sensor == "RGB")

# Merge by whale, event, and printID
df_compare <- orientation_tir %>%
  left_join(
    orientation_rgb,
    by = c("name", "event", "printID"),
    suffix = c("_tir", "_rgb")
  ) %>%
  mutate(
    # Raw absolute heading difference (with circular correction)
    heading_diff = abs(heading_tir - heading_rgb),
    heading_diff = pmin(heading_diff, 360 - heading_diff),
    # Normalize headings to 0–360 scale
    heading_360_tir = (heading_tir + 360) %% 360,
    heading_360_rgb = (heading_rgb + 360) %% 360,
    # Agreement categories
    agreement = case_when(
      heading_diff <= 5   ~ "Excellent",
      heading_diff <= 15  ~ "Good",
      heading_diff <= 20  ~ "Acceptable",
      TRUE                ~ "Discrepancy"
    )
  )

df_compare$agreement <- factor(df_compare$agreement,
                               levels = c("Excellent", "Good", "Acceptable", "Discrepancy"))


# ---------------------- 2. Scatterplot: RGB vs TIR ---------------------- #
p_scatter <- ggplot(df_compare, aes(x = heading_360_rgb, y = heading_360_tir, color = agreement)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(x = "RGB Heading (°)", y = "TIR Heading (°)",
       title = "Comparison of Whale Heading: RGB vs TIR",
       color = "Agreement") +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Excellent" = "chartreuse3",
                                "Good" = "gold",
                                "Acceptable" = "chocolate1",
                                "Discrepancy" = "red"))

# ggsave("results/plots/orientation_scatter.png", p_scatter, width = 7, height = 6)


# ---------------------- 3. Circular Analysis ---------------------- #
rgb <- circular(df_compare$heading_360_rgb, units = "degrees",
                template = "geographics", modulo = "2pi")
tir <- circular(df_compare$heading_360_tir, units = "degrees",
                template = "geographics", modulo = "2pi")

# Descriptive statistics
cat("Circular mean (RGB):", mean(rgb), "\n")
cat("Circular mean (TIR):", mean(tir), "\n")
cat("Circular SD (RGB):", sd.circular(rgb), "\n")
cat("Circular SD (TIR):", sd.circular(tir), "\n")

# Circular correlation
cor_test <- cor.circular(rgb, tir, test = TRUE)
print(cor_test)

# Differences mapped to -180..180
diffs <- (df_compare$heading_360_tir - df_compare$heading_360_rgb + 360) %% 360
diffs <- ifelse(diffs > 180, diffs - 360, diffs)
diffs_circ <- circular(diffs, units = "degrees")

cat("Mean difference (TIR - RGB):", mean(diffs_circ), "\n")
cat("SD of differences:", sd.circular(diffs_circ), "\n")

# Rayleigh test: are differences clustered?
rayleigh_res <- rayleigh.test(diffs_circ)
print(rayleigh_res)


# ---------------------- 4. Agreement Summaries ---------------------- #
df_compare <- df_compare %>%
  mutate(
    diff_raw = (heading_360_tir - heading_360_rgb + 360) %% 360,
    diff_deg = ifelse(diff_raw > 180, diff_raw - 360, diff_raw)
  )

# Per-whale
per_whale_summary <- df_compare %>%
  group_by(name) %>%
  summarise(
    mean_diff = mean(diff_deg),
    sd_diff   = sd(diff_deg),
    n         = n()
  )
print(per_whale_summary)

# Global
global_summary <- df_compare %>%
  summarise(
    mean_diff = mean(diff_deg),
    sd_diff   = sd(diff_deg),
    n         = n()
  )
print(global_summary)

# Agreement distribution
agreement_summary <- df_compare %>%
  count(agreement) %>%
  mutate(percent = 100 * n / sum(n))
print(agreement_summary)


# ---------------------- 5. Plots: Agreement Distributions ---------------------- #
# Histogram (linear scale)
p_hist <- ggplot(df_compare, aes(x = diff_deg, fill = agreement)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.8, boundary = 0) +
  scale_fill_manual(values = c("Excellent" = "chartreuse3",
                               "Good" = "gold",
                               "Acceptable" = "chocolate1",
                               "Discrepancy" = "red")) +
  labs(title = "Distribution of TIR – RGB Heading Differences",
       x = "Difference (°)", y = "Count", fill = "Agreement") +
  theme_minimal(base_size = 14)

# ggsave("results/plots/orientation_hist.png", p_hist, width = 7, height = 6)

#Circular distribution (Supp. Material)
par(mfrow=c(1,2))
plot(rgb, stack=TRUE, bins=360, main="RGB headings")
plot(tir, stack=TRUE, bins=360, main="TIR headings")

mean_rgb <- mean.circular(rgb)
mean_tir <- mean.circular(tir)

plot(rgb, stack=TRUE, bins=360, main="RGB headings")
arrows.circular(mean_rgb, col="blue", lwd=2)

plot(tir, stack=TRUE, bins=360, main="TIR headings")
arrows.circular(mean_tir, col="red", lwd=2)
