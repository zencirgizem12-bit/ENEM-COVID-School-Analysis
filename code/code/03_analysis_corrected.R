# --------------------------------------
# 03_analysis_corrected.R
# Corrected analysis after Matheus's fixes
# --------------------------------------

library(data.table)
library(ggplot2)
library(fixest)
library(patchwork)

rm(list=ls())

# FIX FILE PATH - read from Downloads folder
# (adjust to your own username)
city_panel <- readRDS("C:/Users/Admin/Downloads/city_panel_2017_2021.rds")

# If the above doesn't work, try:
# setwd("C:/Users/Admin/Downloads")
# city_panel <- readRDS("city_panel_2017_2021.rds")

# Check which years are present
cat("Years in data:", unique(city_panel$year), "\n")
cat("Number of observations:", nrow(city_panel), "\n")
cat("Number of municipalities:", length(unique(city_panel$city_code)), "\n")

# Create output directories
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ======================================
# 1. DESCRIPTIVE STATISTICS
# ======================================

# Summary table by year
summary_by_year <- city_panel[, .(
  municipalities = .N,
  students_total = sum(n_students, na.rm = TRUE),
  math_mean = mean(math_mean, na.rm = TRUE),
  science_mean = mean(science_mean, na.rm = TRUE),
  human_mean = mean(human_mean, na.rm = TRUE),
  language_mean = mean(language_mean, na.rm = TRUE),
  essay_mean = mean(essay_mean, na.rm = TRUE),
  pct_full_remote = mean(pct_full_remote, na.rm = TRUE) * 100,
  pct_no_internet = mean(pct_no_internet, na.rm = TRUE) * 100,
  income_rank = mean(income_median, na.rm = TRUE),
  pct_private_school = mean(school_type_private, na.rm = TRUE) * 100
), by = year]

# Round for presentation
summary_by_year[, (2:ncol(summary_by_year)) := lapply(.SD, round, 1), .SDcols = 2:ncol(summary_by_year)]
print(summary_by_year)
fwrite(summary_by_year, "output/tables/summary_by_year.csv")

# ======================================
# 2. FIGURE 1: MATH TREND (2017-2021)
# ======================================

trend_data <- city_panel[, .(
  mean_math = mean(math_mean, na.rm = TRUE),
  se_math = sd(math_mean, na.rm = TRUE) / sqrt(.N),
  mean_essay = mean(essay_mean, na.rm = TRUE),
  se_essay = sd(essay_mean, na.rm = TRUE) / sqrt(.N)
), by = year]

trend_data[, `:=`(
  math_lower = mean_math - 1.96 * se_math,
  math_upper = mean_math + 1.96 * se_math,
  essay_lower = mean_essay - 1.96 * se_essay,
  essay_upper = mean_essay + 1.96 * se_essay
)]

p1 <- ggplot(trend_data, aes(x = year)) +
  geom_line(aes(y = mean_math, color = "Mathematics"), linewidth = 1.2) +
  geom_point(aes(y = mean_math, color = "Mathematics"), size = 3) +
  geom_ribbon(aes(ymin = math_lower, ymax = math_upper, fill = "Mathematics"), alpha = 0.2) +
  geom_line(aes(y = mean_essay, color = "Essay"), linewidth = 1.2) +
  geom_point(aes(y = mean_essay, color = "Essay"), size = 3) +
  geom_ribbon(aes(ymin = essay_lower, ymax = essay_upper, fill = "Essay"), alpha = 0.2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = 2020.5, y = 535, label = "Pandemic Onset", size = 4, color = "red") +
  labs(title = "Average ENEM Scores by Year (2017-2021)",
       x = "Year", y = "Score",
       color = "Subject", fill = "Subject") +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/figure1_score_trends.png", p1, width = 8, height = 5, dpi = 300)
cat("✅ Figure 1 saved\n")

# ======================================
# 3. FIGURE 2: INTERNET QUALITY TREND (CORRECTED)
# ======================================

internet_trend <- city_panel[, .(
  no_internet = mean(pct_no_internet, na.rm = TRUE) * 100,
  mobile = mean(pct_mobile_internet, na.rm = TRUE) * 100,
  full_remote = mean(pct_full_remote, na.rm = TRUE) * 100
), by = year]

# Reshape for plotting
internet_long <- melt(internet_trend, id.vars = "year", 
                      variable.name = "internet_type", 
                      value.name = "percentage")

p2 <- ggplot(internet_long, aes(x = year, y = percentage, color = internet_type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Internet Quality Among Test-Takers (2017-2021)",
       x = "Year", y = "Proportion of Students (%)",
       color = "Internet Access Type") +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021)) +
  scale_color_manual(values = c("no_internet" = "#D55E00", 
                                "mobile" = "#0072B2", 
                                "full_remote" = "#009E73"),
                     labels = c("No Internet", "Mobile Only", "Full Remote (Computer+Internet)")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/figure2_internet_quality.png", p2, width = 8, height = 5, dpi = 300)
cat("✅ Figure 2 saved\n")

# ======================================
# 4. FIGURE 3: PUBLIC VS PRIVATE SCHOOL GAP (CORRECTED)
# ======================================

# First check if variables exist
if(all(c("private_math_mean", "public_math_mean", "n_private", "n_public") %in% names(city_panel))) {
  school_trend <- city_panel[, .(
    private = weighted.mean(private_math_mean, n_private, na.rm = TRUE),
    public = weighted.mean(public_math_mean, n_public, na.rm = TRUE),
    gap = weighted.mean(private_math_mean, n_private, na.rm = TRUE) - 
      weighted.mean(public_math_mean, n_public, na.rm = TRUE)
  ), by = year]
  
  p3 <- ggplot(school_trend, aes(x = year)) +
    geom_line(aes(y = private, color = "Private Schools"), linewidth = 1.2) +
    geom_point(aes(y = private, color = "Private Schools"), size = 3) +
    geom_line(aes(y = public, color = "Public Schools"), linewidth = 1.2) +
    geom_point(aes(y = public, color = "Public Schools"), size = 3) +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "red", alpha = 0.7) +
    labs(title = "Math Scores: Private vs Public Schools",
         x = "Year", y = "Average Math Score",
         color = "School Type") +
    scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("output/figures/figure3_school_gap.png", p3, width = 8, height = 5, dpi = 300)
  cat("✅ Figure 3 saved\n")
  print(school_trend)
} else {
  cat("⚠️ Warning: private_math_mean or public_math_mean not found in data\n")
  cat("Available columns:", paste(names(city_panel), collapse=", "), "\n")
}

# ======================================
# 5. FIGURE 4: MATH VS INTERNET (CORRECTED - POSITIVE CORRELATION)
# ======================================

# Calculate correlation by year
cor_by_year <- city_panel[, .(
  correlation = cor(pct_full_remote, math_mean, use = "complete.obs"),
  n_cities = .N
), by = year]

print("Correlations by year:")
print(cor_by_year)

p4 <- ggplot(city_panel, aes(x = pct_full_remote * 100, y = math_mean, color = as.factor(year))) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  labs(title = "Math Score vs Full Remote Learning Capacity",
       x = "% Students with Computer + Internet",
       y = "Average Math Score",
       color = "Year") +
  scale_x_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figures/figure4_math_vs_internet.png", p4, width = 8, height = 5, dpi = 300)
cat("✅ Figure 4 saved\n")

# ======================================
# 6. FIGURE 5: INCOME TREND (SELECTION BIAS)
# ======================================

income_trend <- city_panel[, .(
  income_mean = mean(income_median, na.rm = TRUE),
  income_se = sd(income_median, na.rm = TRUE) / sqrt(.N)
), by = year]

income_trend[, `:=`(lower = income_mean - 1.96 * income_se,
                    upper = income_mean + 1.96 * income_se)]

p5 <- ggplot(income_trend, aes(x = year, y = income_mean)) +
  geom_line(linewidth = 1.2, color = "darkred") +
  geom_point(size = 3, color = "darkred") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "darkred") +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Mean Income Rank Among Test-Takers (2017-2021)",
       x = "Year", 
       y = "Mean Income Rank (higher = richer students)") +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021)) +
  theme_minimal()

ggsave("output/figures/figure5_income_trend.png", p5, width = 8, height = 5, dpi = 300)
cat("✅ Figure 5 saved\n")

# ======================================
# 7. FIGURE 6: EVENT STUDY (CORRECTED)
# ======================================

# Create treatment groups based on pre-pandemic internet quality (2017)
internet_2017 <- city_panel[year == 2017, .(city_code, internet_2017 = pct_full_remote)]
city_panel <- merge(city_panel, internet_2017, by = "city_code", all.x = TRUE)

# High-internet indicator (top 25%)
high_threshold <- quantile(city_panel[year == 2017, internet_2017], 0.75, na.rm = TRUE)
city_panel[, high_internet := internet_2017 > high_threshold]

cat("High internet threshold:", high_threshold, "\n")
cat("Number of high-internet cities:", sum(city_panel[year==2017, high_internet], na.rm=TRUE), "\n")

# Event study model (using 2019 as reference)
event_model <- feols(math_mean ~ i(year, high_internet, ref = 2019) | 
                       city_code + year, 
                     data = city_panel,
                     cluster = ~city_code)

saveRDS(event_model, "output/results/event_study_model.rds")

# Extract coefficients for plotting
coef_values <- coef(event_model)
coef_se <- se(event_model)
year_terms <- grep("year::.*:high_internet", names(coef_values), value = TRUE)

coef_plot <- data.table(
  year = as.numeric(gsub("year::([0-9]+):.*", "\\1", year_terms)),
  estimate = coef_values[year_terms],
  std_error = coef_se[year_terms]
)
coef_plot[, `:=`(lower = estimate - 1.96 * std_error,
                 upper = estimate + 1.96 * std_error)]

print("Event study coefficients:")
print(coef_plot)

p6 <- ggplot(coef_plot, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_point(size = 4, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.8) +
  labs(title = "Event Study: High vs Low Internet Cities",
       x = "Year", 
       y = "Treatment Effect (Math Score Difference)") +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021)) +
  theme_minimal() +
  annotate("text", x = 2020.5, y = max(coef_plot$upper, na.rm=TRUE), 
           label = "Pandemic Onset", size = 4, color = "red", hjust = 0)

ggsave("output/figures/figure6_event_study.png", p6, width = 8, height = 5, dpi = 300)
cat("✅ Figure 6 saved\n")

# ======================================
# 8. FINAL SUMMARY
# ======================================

cat("\n========================================\n")
cat("✅ ALL ANALYSIS COMPLETE!\n")
cat("========================================\n")
cat("Files saved to output/figures/\n")
cat("- figure1_score_trends.png\n")
cat("- figure2_internet_quality.png\n")
cat("- figure3_school_gap.png\n")
cat("- figure4_math_vs_internet.png\n")
cat("- figure5_income_trend.png\n")
cat("- figure6_event_study.png\n")
cat("\nTables saved to output/tables/\n")
cat("- summary_by_year.csv\n")
cat("- event_study_model.rds\n")

# Create missing folder
dir.create("output/results", recursive = TRUE, showWarnings = FALSE)

# Now save the model
saveRDS(event_model, "output/results/event_study_model.rds")
cat("✅ Model saved\n")

# Extract coefficients for plotting
coef_values <- coef(event_model)
coef_se <- se(event_model)
year_terms <- grep("year::.*:high_internet", names(coef_values), value = TRUE)

coef_plot <- data.table(
  year = as.numeric(gsub("year::([0-9]+):.*", "\\1", year_terms)),
  estimate = coef_values[year_terms],
  std_error = coef_se[year_terms]
)
coef_plot[, `:=`(lower = estimate - 1.96 * std_error,
                 upper = estimate + 1.96 * std_error)]

print("Event study coefficients:")
print(coef_plot)

p6 <- ggplot(coef_plot, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_point(size = 4, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.8) +
  labs(title = "Event Study: High vs Low Internet Cities",
       x = "Year", 
       y = "Treatment Effect (Math Score Difference)") +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020, 2021)) +
  theme_minimal() +
  annotate("text", x = 2020.5, y = max(coef_plot$upper, na.rm=TRUE), 
           label = "Pandemic Onset", size = 4, color = "red", hjust = 0)

ggsave("output/figures/figure6_event_study.png", p6, width = 8, height = 5, dpi = 300)
cat("✅ Figure 6 saved\n")

# ======================================
# 8. FINAL SUMMARY
# ======================================

cat("\n========================================\n")
cat("✅ ALL ANALYSIS COMPLETE!\n")
cat("========================================\n")
cat("Files saved to output/figures/\n")
cat("- figure1_score_trends.png\n")
cat("- figure2_internet_quality.png\n")
cat("- figure3_school_gap.png\n")
cat("- figure4_math_vs_internet.png\n")
cat("- figure5_income_trend.png\n")
cat("- figure6_event_study.png\n")
cat("\nTables saved to output/tables/\n")
cat("- summary_by_year.csv\n")
cat("\nModel saved to output/results/\n")
cat("- event_study_model.rds\n")

# Print key findings
cat("\n========================================\n")
cat("KEY FINDINGS:\n")
cat("========================================\n")
cat("1. Private-public school gap:", round(school_trend[year==2021, gap], 1), "points\n")
cat("2. Correlation (math vs internet):", round(cor_by_year[year==2021, correlation], 3), "\n")
cat("3. Income rank increase (2019→2021):", 
    round(income_trend[year==2021, income_mean] - income_trend[year==2019, income_mean], 3), "\n")
cat("4. Event study 2021 coefficient:", round(coef_plot[year==2021, estimate], 2), 
    "±", round(1.96 * coef_plot[year==2021, std_error], 2), "\n")

# Check how the data was generated
# Look at the distribution of internet quality variables

# Internet quality averages by year
city_panel[, .(
  full_remote_mean = mean(pct_full_remote, na.rm = TRUE),
  full_remote_sd = sd(pct_full_remote, na.rm = TRUE),
  full_remote_min = min(pct_full_remote, na.rm = TRUE),
  full_remote_max = max(pct_full_remote, na.rm = TRUE)
), by = year]

# Also look at histogram of raw pct_full_remote variable
hist_data <- city_panel[year == 2021, pct_full_remote]
summary(hist_data)
