source("code/data_utils.R")
library(patchwork)

forecast_date <- "2023-11-12"

ts <- load_combined_series('sari') %>% 
  filter(age_group == 'DE')

rt <- load_rt("sari") %>% 
  filter(date <= forecast_date,
         age_group == "00+")

ts <- load_target_series("sari", forecast_date, "DE", wide=FALSE) %>% 
  filter(date >= "2023-02-01")

ts_cut <- head(ts, -4)

ts_frozen <- rt %>%
  select(-value_1w, -value_2w, -value_3w, -value_4w) %>%
  rename(value = value_0w) %>% 
  filter(date >= "2023-02-01")

p1 <- ggplot(ts, aes(x=date, y=value)) + 
  geom_line() +
  theme_bw()

p2 <- ggplot(ts_cut, aes(x=date, y=value)) + 
  geom_line() +
  theme_bw()

p3 <- ggplot(ts, aes(x=date, y=value)) + 
  geom_rect(xmin=max(ts_cut$date), xmax=Inf, ymin=-Inf, ymax=Inf,
            fill = "gray", alpha = 0.05) +  
  geom_line() +
  theme_bw()

p4 <- ggplot(ts_frozen, aes(x=date, y=value)) + 
  geom_line() +
  theme_bw()

(p1 + p2) / (p4 + p3) &
  scale_x_date(limits = range(ts$date)) &
  scale_y_continuous(limits = c(0, 15000)) & 
  labs(x = NULL, y = "SARI")





