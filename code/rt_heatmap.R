library(tidyverse)
library(patchwork)
Sys.setlocale("LC_TIME", "C")

cutoff_date <- as.Date("2024-01-11")
max_delay <- 4

status_colors <- c(
  "Reported" = "deepskyblue3",
  "Not yet reported" = "darkorange3",
  "Future" = "gray60"
)

# Load and filter
df <- read_csv("data/rt_sari_10w.csv") %>%
  filter(age_group == "00+") %>%
  filter((year == 2023 & week >= 40) | (year == 2024 & week <= 20))

df_long <- df %>%
  pivot_longer(cols = starts_with("value_"),
               names_to = "delay",
               values_to = "value") %>%
  mutate(
    delay = as.integer(str_remove_all(delay, "value_|w")),
    value = ifelse(value == 0, 0.01, value),
    report_date = date + delay * 7,
    complete_date = date + max_delay * 7,
    status = case_when(
      date > cutoff_date ~ "Future",
      report_date >= cutoff_date ~ "Not yet reported",
      TRUE ~ "Reported"
    )
  ) %>%
  filter(delay <= max_delay)

# Aggregate for bar chart
df_agg <- df_long %>% 
  group_by(date, status) %>% 
  summarize(value = sum(value), .groups = "drop") %>%
  mutate(status = factor(status, levels = c("Future", "Not yet reported", "Reported")))

# Plot 1: Reporting trapezoid
p1 <- ggplot(df_long, aes(x = date, y = delay, fill = status, alpha = sqrt(value))) +
  geom_tile() +
  scale_fill_manual(values = status_colors, guide = "none") +
  scale_alpha(range = c(0.1, 1), guide = "none") +
  scale_y_continuous(breaks = 0:max_delay) +
  labs(
    title = paste("Reporting trapezoid –", format(cutoff_date, "%d %b %Y")),
    x = NULL,
    y = "Reporting delay (weeks)",
    fill = NULL
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
  )

# Plot 2: Stacked bar chart
p2 <- ggplot(df_agg, aes(x = date, y = value, fill = status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = status_colors) +
  labs(
    title = paste("Reported cases –", format(cutoff_date, "%d %b %Y")),
    x = NULL,
    y = "Reported cases",
    fill = NULL
  ) +
  theme_bw()

# Combine
p2 / p1 
