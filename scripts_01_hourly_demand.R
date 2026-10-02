# ============================================================
# Alberta Power Market Analysis
# Script 01: Hourly Demand and Pool Price
# Year: 2024
# ============================================================


# 1. Load packages ---------------------------------------------------------

library(tidyverse)
library(lubridate)


# 2. Import AESO hourly market data ----------------------------------------

file_path <- "data/Hourly_Metered_Volumes_and_Pool_Price_and_AIL_2020-Jul2025.csv"

aeso <- read_csv(
  file_path,
  col_select = c(
    Date_Begin_Local,
    ACTUAL_AIL,
    ACTUAL_POOL_PRICE
  ),
  show_col_types = FALSE
)


# 3. Prepare 2024 data -----------------------------------------------------

aeso_2024 <- aeso %>%
  mutate(
    date = as.Date(substr(Date_Begin_Local, 1, 10)),
    
    hour = as.integer(
      str_match(
        Date_Begin_Local,
        " ([0-9]{1,2}):"
      )[, 2]
    )
  ) %>%
  filter(year(date) == 2024)


# Basic quality-control checks

stopifnot(nrow(aeso_2024) == 8784)
stopifnot(sum(is.na(aeso_2024$hour)) == 0)
stopifnot(all(sort(unique(aeso_2024$hour)) == 0:23))


# 4. Average hourly electricity demand ------------------------------------

hourly_demand <- aeso_2024 %>%
  group_by(hour) %>%
  summarise(
    mean_demand = mean(ACTUAL_AIL, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


p_demand <- ggplot(
  hourly_demand,
  aes(x = hour, y = mean_demand)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2)
  ) +
  labs(
    title = "Average Hourly Electricity Demand in Alberta",
    subtitle = "2024",
    x = "Hour of Day",
    y = "Average Demand (MW)"
  ) +
  theme_minimal()


ggsave(
  "figures/01_hourly_demand_2024.png",
  plot = p_demand,
  width = 10,
  height = 6,
  dpi = 300
)


# 5. Average hourly pool price ---------------------------------------------

hourly_market <- aeso_2024 %>%
  group_by(hour) %>%
  summarise(
    mean_demand = mean(ACTUAL_AIL, na.rm = TRUE),
    mean_price = mean(ACTUAL_POOL_PRICE, na.rm = TRUE),
    median_price = median(ACTUAL_POOL_PRICE, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )


p_price <- ggplot(
  hourly_market,
  aes(x = hour, y = mean_price)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2)
  ) +
  labs(
    title = "Average Hourly Pool Price in Alberta",
    subtitle = "2024",
    x = "Hour of Day",
    y = "Average Pool Price ($/MWh)"
  ) +
  theme_minimal()


ggsave(
  "figures/02_hourly_pool_price_2024.png",
  plot = p_price,
  width = 10,
  height = 6,
  dpi = 300
)


# 6. Compare mean and median pool prices ----------------------------------

price_comparison <- hourly_market %>%
  select(
    hour,
    mean_price,
    median_price
  ) %>%
  pivot_longer(
    cols = c(mean_price, median_price),
    names_to = "statistic",
    values_to = "price"
  )


p_price_comparison <- ggplot(
  price_comparison,
  aes(
    x = hour,
    y = price,
    color = statistic
  )
) +
  geom_line(linewidth = 1) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2)
  ) +
  labs(
    title = "Mean vs Median Pool Price",
    subtitle = "Alberta, 2024",
    x = "Hour of Day",
    y = "Pool Price ($/MWh)",
    color = "Statistic"
  ) +
  theme_minimal()


ggsave(
  "figures/03_mean_vs_median_pool_price_2024.png",
  plot = p_price_comparison,
  width = 10,
  height = 6,
  dpi = 300
)