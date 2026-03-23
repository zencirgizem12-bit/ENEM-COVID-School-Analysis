# --------------------------------------
# 02_analysis.R
# Event study and visualizations
# --------------------------------------

library(data.table)
library(ggplot2)
library(fixest)

# Load panel data
city_panel <- readRDS("data/processed/city_panel_2017_2021.rds")

# ======================================
# 1. EVENT STUDY
# ======================================

# Create treatment groups based on pre-pandemic internet quality (2017)
internet_2017 <- city_panel[year == 2017, .(city_code, internet_2017 = pct_full_remote)]
city_panel <- merge(city_panel, internet_2017, by = "city_code", all.x = TRUE)

# High-internet indicator (top 25%)
high_threshold <- quantile(city_panel[year == 2017, internet_2017], 0.75, na.rm = TRUE)
city_panel[, high_internet := internet_2017 > high_threshold]

# Event study model
event_model <- feols(math_mean ~ i(year, high_internet, ref = 2019) | 
                       city_code + year, 
                     data = city_panel)

# Save results
saveRDS(event_model, "output/results/event_study_model.rds")

# ======================================
# 2. FIGURES
# ======================================

# Figure 1: Math trend
trend_data <- city_panel[, .(
  mean_math = mean(math_mean, na.rm = TRUE),
  se_math = sd(math_mean, na.rm = TRUE) / sqrt(.N)
), by = year]

trend_data[, lower := mean_math - 1.96 * se_math]
trend_data[, upper := mean_math + 1.96 * se_math]

p1 <- ggplot(trend_data, aes(x = year, y = mean_math)) +
  geom_line(linewidth = 1.2, color = "darkblue") +
  geom_point(size = 3, color = "darkblue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Average Math Score by Year (2017-2021)",
       x = "Year", y = "Math Score") +
  theme_minimal()

ggsave("output/figures/math_trend.png", p1, width = 8, height = 5, dpi = 300)

# Figure 2: Internet quality trend
internet_trend <- city_panel[, .(
  no_internet = mean(pct_no_internet, na.rm = TRUE),
  full_remote = mean(pct_full_remote, na.rm = TRUE)
), by = year]

p2 <- ggplot(internet_trend, aes(x = year)) +
  geom_line(aes(y = no_internet, color = "No Internet"), linewidth = 1.2) +
  geom_line(aes(y = full_remote, color = "Full Remote"), linewidth = 1.2) +
  labs(title = "Internet Access Quality Over Time",
       x = "Year", y = "Proportion of Students",
       color = "Internet Quality") +
  theme_minimal()

ggsave("output/figures/internet_quality_trend.png", p2, width = 8, height = 5, dpi = 300)

# Figure 3: School type comparison
school_trend <- city_panel[, .(
  private = weighted.mean(math_mean, school_type_private, na.rm = TRUE),
  public = weighted.mean(math_mean, 1 - school_type_private, na.rm = TRUE)
), by = year]

p3 <- ggplot(school_trend, aes(x = year)) +
  geom_line(aes(y = private, color = "Private Schools"), linewidth = 1.2) +
  geom_line(aes(y = public, color = "Public Schools"), linewidth = 1.2) +
  labs(title = "Math Scores: Private vs Public Schools",
       x = "Year", y = "Average Math Score",
       color = "School Type") +
  theme_minimal()

ggsave("output/figures/school_type_trend.png", p3, width = 8, height = 5, dpi = 300)

# Figure 4: Event study coefficients
coef_values <- coef(event_model)
coef_se <- se(event_model)

year_terms <- grep("year::.*:high_internet", names(coef_values), value = TRUE)

coef_plot <- data.table(
  year = as.numeric(gsub("year::([0-9]+):.*", "\\1", year_terms)),
  estimate = coef_values[year_terms],
  std_error = coef_se[year_terms]
)

coef_plot[, lower := estimate - 1.96 * std_error]
coef_plot[, upper := estimate + 1.96 * std_error]

p4 <- ggplot(coef_plot, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red") +
  geom_point(size = 4, color = "darkblue") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "Event Study: High vs Low Internet Cities",
       subtitle = "Red line = Pandemic onset (2020)",
       x = "Year", y = "Treatment Effect") +
  theme_minimal()

ggsave("output/figures/event_study.png", p4, width = 8, height = 5, dpi = 300)

# ======================================
# 3. RESULTS TABLE
# ======================================

# Create summary table
results_table <- coef_plot[, .(
  Year = year,
  Effect = round(estimate, 2),
  SE = round(std_error, 2),
  CI = paste0("[", round(lower, 2), ", ", round(upper, 2), "]")
)]

fwrite(results_table, "output/tables/event_study_results.csv")

cat("\n✅ Analysis complete! Figures saved to output/figures/\n")
