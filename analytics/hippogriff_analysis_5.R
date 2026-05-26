# =============================================================================
# Hippogriff Group - Greenhouse Gas Concentrations & Temperature in Tasmania
# A Comparative Analysis of CO2, CH4 & N2O (1996-2025)
# =============================================================================
#
# GitHub Repository Structure:
#   data/        -> raw CSV files (CO2.csv, CH4.csv, N2O.csv, Temp.csv)
#   analytics/   -> this R script
#   figures/     -> all plots saved as PNG
#   outputs/     -> descriptive stats and model results saved as CSV/txt
#
# Run this script from the root of the repository.
# =============================================================================

library(ggplot2)
library(car)

# -----------------------------------------------------------------------------
# FOLDER PATHS
# -----------------------------------------------------------------------------

data_dir    <- "data"
figures_dir <- "figures"
outputs_dir <- "outputs"

# -----------------------------------------------------------------------------
# 1. LOAD & MERGE DATA
# -----------------------------------------------------------------------------

co2  <- read.csv(file.path(data_dir, "CO2.csv"))
ch4  <- read.csv(file.path(data_dir, "CH4.csv"))
n2o  <- read.csv(file.path(data_dir, "N2O.csv"))
temp <- read.csv(file.path(data_dir, "Temp.csv"))

names(co2)  <- c("Year", "Month", "CO2")
names(ch4)  <- c("Year", "Month", "CH4")
names(n2o)  <- c("Year", "Month", "N2O")
names(temp) <- c("Year", "Month", "Temp")

monthly <- merge(co2,     ch4,  by = c("Year", "Month"))
monthly <- merge(monthly, n2o,  by = c("Year", "Month"))
monthly <- merge(monthly, temp, by = c("Year", "Month"))

annual <- aggregate(cbind(CO2, CH4, N2O, Temp) ~ Year, data = monthly, FUN = mean)

monthly$Time <- monthly$Year + (monthly$Month - 1) / 12

# -----------------------------------------------------------------------------
# SHARED THEME
# -----------------------------------------------------------------------------

pres_theme <- theme_bw(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(size = 11),
    plot.title       = element_text(size = 13, face = "bold"),
    plot.subtitle    = element_text(size = 10, colour = "grey40")
  )

# -----------------------------------------------------------------------------
# 2. DESCRIPTIVE STATISTICS  ->  outputs/
# -----------------------------------------------------------------------------

desc <- data.frame(
  Variable = c("CO2 (ppm)", "CH4 (ppb)", "N2O (ppb)", "Temperature (C)"),
  Mean     = c(mean(monthly$CO2),  mean(monthly$CH4),  mean(monthly$N2O),  mean(monthly$Temp)),
  Median   = c(median(monthly$CO2),median(monthly$CH4),median(monthly$N2O),median(monthly$Temp)),
  SD       = c(sd(monthly$CO2),    sd(monthly$CH4),    sd(monthly$N2O),    sd(monthly$Temp)),
  Min      = c(min(monthly$CO2),   min(monthly$CH4),   min(monthly$N2O),   min(monthly$Temp)),
  Q1       = c(quantile(monthly$CO2, 0.25), quantile(monthly$CH4, 0.25),
               quantile(monthly$N2O, 0.25), quantile(monthly$Temp, 0.25)),
  Q3       = c(quantile(monthly$CO2, 0.75), quantile(monthly$CH4, 0.75),
               quantile(monthly$N2O, 0.75), quantile(monthly$Temp, 0.75)),
  Max      = c(max(monthly$CO2),   max(monthly$CH4),   max(monthly$N2O),   max(monthly$Temp)),
  IQR      = c(IQR(monthly$CO2),   IQR(monthly$CH4),   IQR(monthly$N2O),   IQR(monthly$Temp))
)

desc[, -1] <- round(desc[, -1], 3)
write.csv(desc, file.path(outputs_dir, "descriptive_statistics.csv"), row.names = FALSE)

# Also print to console
print(desc)

# -----------------------------------------------------------------------------
# 3. TREND ANALYSIS  ->  figures/
# -----------------------------------------------------------------------------

# Temperature
p_temp <- ggplot(annual, aes(x = Year, y = Temp)) +
  geom_line(colour = "black", linewidth = 0.8) +
  labs(title = "Yearly Average Temperature Trend (1996-2025)",
       x = "Year", y = "Yearly Average Temperature (°C)") +
  pres_theme

ggsave(file.path(figures_dir, "trend_temperature.png"),
       plot = p_temp, width = 8, height = 5, dpi = 300)

# CO2
p_co2 <- ggplot(monthly, aes(x = Time, y = CO2)) +
  geom_line(colour = "black", linewidth = 0.5) +
  labs(title = "CO2 Concentration Trend (1996-2025)",
       x = "Year", y = "CO2 (ppm)") +
  pres_theme

ggsave(file.path(figures_dir, "trend_CO2.png"),
       plot = p_co2, width = 8, height = 5, dpi = 300)

# CH4
p_ch4 <- ggplot(monthly, aes(x = Time, y = CH4)) +
  geom_line(colour = "black", linewidth = 0.5) +
  labs(title = "CH4 Concentration Trend (1996-2025)",
       x = "Year", y = "CH4 (ppb)") +
  pres_theme

ggsave(file.path(figures_dir, "trend_CH4.png"),
       plot = p_ch4, width = 8, height = 5, dpi = 300)

# N2O
p_n2o <- ggplot(monthly, aes(x = Time, y = N2O)) +
  geom_line(colour = "black", linewidth = 0.5) +
  labs(title = "N2O Concentration Trend (1996-2025)",
       x = "Year", y = "N2O (ppb)") +
  pres_theme

ggsave(file.path(figures_dir, "trend_N2O.png"),
       plot = p_n2o, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 4. NORMALITY TESTING - Q-Q PLOTS  ->  figures/
# -----------------------------------------------------------------------------

p_qq_co2 <- ggplot(monthly, aes(sample = CO2)) +
  stat_qq(colour = "black", size = 1) +
  stat_qq_line(colour = "black", linetype = "dashed") +
  labs(title = "Q-Q Plot: CO2 Concentration",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  pres_theme

ggsave(file.path(figures_dir, "qq_CO2.png"),
       plot = p_qq_co2, width = 6, height = 5, dpi = 300)

p_qq_temp <- ggplot(monthly, aes(sample = Temp)) +
  stat_qq(colour = "black", size = 1) +
  stat_qq_line(colour = "black", linetype = "dashed") +
  labs(title = "Q-Q Plot: Temperature",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  pres_theme

ggsave(file.path(figures_dir, "qq_temperature.png"),
       plot = p_qq_temp, width = 6, height = 5, dpi = 300)

p_qq_ch4 <- ggplot(monthly, aes(sample = CH4)) +
  stat_qq(colour = "black", size = 1) +
  stat_qq_line(colour = "black", linetype = "dashed") +
  labs(title = "Q-Q Plot: CH4 Concentration",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  pres_theme

ggsave(file.path(figures_dir, "qq_CH4.png"),
       plot = p_qq_ch4, width = 6, height = 5, dpi = 300)

p_qq_n2o <- ggplot(monthly, aes(sample = N2O)) +
  stat_qq(colour = "black", size = 1) +
  stat_qq_line(colour = "black", linetype = "dashed") +
  labs(title = "Q-Q Plot: N2O Concentration",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  pres_theme

ggsave(file.path(figures_dir, "qq_N2O.png"),
       plot = p_qq_n2o, width = 6, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# 5. PEARSON CORRELATION  ->  outputs/
# -----------------------------------------------------------------------------

cor_matrix <- cor(monthly[, c("CO2", "CH4", "N2O", "Temp")])
write.csv(round(cor_matrix, 4), file.path(outputs_dir, "pearson_correlation_matrix.csv"))

# P-values
cor_pvalues <- data.frame(
  Pair    = c("CO2 vs CH4", "CO2 vs N2O", "CO2 vs Temp",
              "CH4 vs N2O", "CH4 vs Temp", "N2O vs Temp"),
  r       = round(c(
    cor.test(monthly$CO2, monthly$CH4)$estimate,
    cor.test(monthly$CO2, monthly$N2O)$estimate,
    cor.test(monthly$CO2, monthly$Temp)$estimate,
    cor.test(monthly$CH4, monthly$N2O)$estimate,
    cor.test(monthly$CH4, monthly$Temp)$estimate,
    cor.test(monthly$N2O, monthly$Temp)$estimate
  ), 4),
  p_value = c(
    cor.test(monthly$CO2, monthly$CH4)$p.value,
    cor.test(monthly$CO2, monthly$N2O)$p.value,
    cor.test(monthly$CO2, monthly$Temp)$p.value,
    cor.test(monthly$CH4, monthly$N2O)$p.value,
    cor.test(monthly$CH4, monthly$Temp)$p.value,
    cor.test(monthly$N2O, monthly$Temp)$p.value
  )
)

write.csv(cor_pvalues, file.path(outputs_dir, "pearson_correlation_pvalues.csv"), row.names = FALSE)

print(cor_matrix)
print(cor_pvalues)

# -----------------------------------------------------------------------------
# 6. SIMPLE LINEAR REGRESSION  ->  outputs/ + figures/
# -----------------------------------------------------------------------------

# Model 1: CH4 -> CO2
fit1 <- lm(CO2 ~ CH4, data = monthly)
summary(fit1)

p_reg1 <- ggplot(monthly, aes(x = CH4, y = CO2)) +
  geom_point(shape = 16, size = 1.2, colour = "grey30", alpha = 0.5) +
  geom_smooth(method = "lm", colour = "black", fill = "grey70",
              linewidth = 0.9, se = TRUE) +
  labs(title = "CH4 vs CO2",
       x = "CH4 Concentration (ppb)", y = "CO2 Concentration (ppm)") +
  pres_theme

ggsave(file.path(figures_dir, "regression_CH4_vs_CO2.png"),
       plot = p_reg1, width = 7, height = 5, dpi = 300)

# Model 2: CH4 -> N2O
fit2 <- lm(N2O ~ CH4, data = monthly)
summary(fit2)

p_reg2 <- ggplot(monthly, aes(x = CH4, y = N2O)) +
  geom_point(shape = 16, size = 1.2, colour = "grey30", alpha = 0.5) +
  geom_smooth(method = "lm", colour = "black", fill = "grey70",
              linewidth = 0.9, se = TRUE) +
  labs(title = "CH4 vs N2O",
       x = "CH4 Concentration (ppb)", y = "N2O Concentration (ppb)") +
  pres_theme

ggsave(file.path(figures_dir, "regression_CH4_vs_N2O.png"),
       plot = p_reg2, width = 7, height = 5, dpi = 300)

# Model 3: CO2 -> N2O
fit3 <- lm(N2O ~ CO2, data = monthly)
summary(fit3)

p_reg3 <- ggplot(monthly, aes(x = CO2, y = N2O)) +
  geom_point(shape = 16, size = 1.2, colour = "grey30", alpha = 0.5) +
  geom_smooth(method = "lm", colour = "black", fill = "grey70",
              linewidth = 0.9, se = TRUE) +
  labs(title = "CO2 vs N2O",
       x = "CO2 Concentration (ppm)", y = "N2O Concentration (ppb)") +
  pres_theme

ggsave(file.path(figures_dir, "regression_CO2_vs_N2O.png"),
       plot = p_reg3, width = 7, height = 5, dpi = 300)

# Save regression results
reg_results <- data.frame(
  Model       = c("CH4 -> CO2", "CH4 -> N2O", "CO2 -> N2O"),
  Intercept   = round(c(coef(fit1)[1], coef(fit2)[1], coef(fit3)[1]), 4),
  Beta        = round(c(coef(fit1)[2], coef(fit2)[2], coef(fit3)[2]), 4),
  R_squared   = round(c(summary(fit1)$r.squared,
                         summary(fit2)$r.squared,
                         summary(fit3)$r.squared), 4),
  p_value     = c(summary(fit1)$coefficients[2, 4],
                  summary(fit2)$coefficients[2, 4],
                  summary(fit3)$coefficients[2, 4])
)

write.csv(reg_results, file.path(outputs_dir, "simple_regression_results.csv"), row.names = FALSE)
print(reg_results)

# -----------------------------------------------------------------------------
# 7. MULTIPLE LINEAR REGRESSION  ->  outputs/ + figures/
# -----------------------------------------------------------------------------

mlr <- lm(Temp ~ CH4 + CO2 + N2O, data = monthly)
summary(mlr)

monthly$Predicted_Temp <- fitted(mlr)

p_mlr <- ggplot(monthly, aes(x = Temp, y = Predicted_Temp)) +
  geom_point(shape = 16, size = 1.2, colour = "grey30", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, colour = "black", linewidth = 1) +
  labs(title = "Actual vs Predicted Temperature",
       x = "Actual Temperature (°C)", y = "Predicted Temperature (°C)") +
  pres_theme

ggsave(file.path(figures_dir, "regression_actual_vs_predicted.png"),
       plot = p_mlr, width = 7, height = 5, dpi = 300)

# Save MLR results
mlr_coefs <- as.data.frame(summary(mlr)$coefficients)
mlr_coefs$VIF <- c(NA, vif(mlr))
write.csv(round(mlr_coefs, 4),
          file.path(outputs_dir, "multiple_regression_results.csv"))

print(summary(mlr))
print(vif(mlr))

