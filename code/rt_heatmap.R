library(tidyverse)
library(patchwork)
Sys.setlocale("LC_TIME", "C")

cutoff_date <- as.Date("2024-01-11")
#cutoff_date <- as.Date("2023-12-28")
max_delay <- 8
n_colors <- 100

# Color gradients
blue_gradient    <- colorRampPalette(c("white", "deepskyblue3"))(n_colors) # "aliceblue"
orange_gradient  <- colorRampPalette(c("white", "darkorange3"))(n_colors) # "#ffe6cc"
gray_gradient    <- colorRampPalette(c("white", "gray20"))(n_colors)

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
      date > cutoff_date ~ "future",
      report_date >= cutoff_date ~ "incomplete",
      TRUE ~ "complete"
    )
  ) %>%
  filter(delay <= max_delay)

# Assign colors
df_long <- df_long %>%
  mutate(
    value_scaled = rescale(sqrt(value), to = c(1, n_colors)) %>% round(),
    fill_color = case_when(
      status == "complete"   ~ blue_gradient[value_scaled],
      status == "incomplete" ~ orange_gradient[value_scaled],
      status == "future"     ~ gray_gradient[value_scaled]
    )
  )


df_agg <- df_long %>% 
  group_by(date, status) %>% 
  summarize(value = sum(value), .groups = "drop") %>%
  mutate(
    status = factor(status, levels = c("future", "incomplete", "complete"),
                    labels = c("Future", "Not yet reported", "Reported"))
  )



# Plot 1: Reporting trapezoid
p1 <- ggplot(df_long, aes(x = date, y = delay)) +
  geom_tile(aes(fill = fill_color)) +
  scale_fill_identity() +
  scale_y_continuous(breaks = 0:max_delay) +
  labs(
    title = paste("Reporting trapezoid –", format(cutoff_date, "%d %b %Y")),
    x = NULL,
    y = "Reporting delay (weeks)"
  ) +
  theme_bw()

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


p2 / p1
