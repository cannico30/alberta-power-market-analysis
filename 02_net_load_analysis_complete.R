# ============================================================
# Alberta Power Market Analysis
# Script 02: Net Load, High-Price Conditions, Supply and Outages
# Year: 2024
# ============================================================

library(tidyverse)
library(lubridate)
library(readxl)

# File paths
main_file <- "data/Hourly_Metered_Volumes_and_Pool_Price_and_AIL_2020-Jul2025.csv"
wind_file <- "data/Wind_Data_2024.csv"
solar_file <- "data/Solar_Data_2024.csv"
asset_file <- "data/CSD-Assets.xlsx"
outage_file <- "data/AESO_Generation_Capacity_Outages_2024.csv"

# Helper: some AESO renewable timestamps are date-only midnight values
parse_aeso_time <- function(x) {
  x <- trimws(x)
  date_only <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x)

  result <- as.POSIXct(
    rep(NA_real_, length(x)),
    origin = "1970-01-01",
    tz = "UTC"
  )

  result[date_only] <- as.POSIXct(
    as.Date(x[date_only]),
    tz = "UTC"
  )

  result[!date_only] <- as.POSIXct(
    x[!date_only],
    format = "%Y-%m-%d %I:%M:%S %p",
    tz = "UTC"
  )

  result
}

# -------------------------------------------------------------------------
# 1. Identify matched generating assets
# -------------------------------------------------------------------------

aeso_header <- read_csv(
  main_file,
  n_max = 0,
  show_col_types = FALSE
)

csd_assets <- read_excel(
  asset_file,
  sheet = "CSD-Assets"
)

asset_columns <- intersect(
  names(aeso_header),
  csd_assets$ASSET_SHORT_NAME
)

asset_coverage <- csd_assets %>%
  mutate(matched = ASSET_SHORT_NAME %in% asset_columns) %>%
  group_by(FUEL_TYPE, matched) %>%
  summarise(
    n_assets = n(),
    total_max_capability = sum(MAXIMUM_CAPABILITY, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# 2. Import 2024 AESO market, intertie and asset data
# -------------------------------------------------------------------------

intertie_columns <- c(
  "IMPORT_BC", "IMPORT_MT", "IMPORT_SK",
  "EXPORT_BC", "EXPORT_MT", "EXPORT_SK"
)

aeso_raw <- read_csv(
  main_file,
  col_select = c(
    Date_Begin_GMT,
    Date_Begin_Local,
    ACTUAL_POOL_PRICE,
    ACTUAL_AIL,
    all_of(intertie_columns),
    all_of(asset_columns)
  ),
  show_col_types = FALSE
) %>%
  filter(startsWith(Date_Begin_Local, "2024-")) %>%
  mutate(
    hour_utc = as.POSIXct(
      Date_Begin_GMT,
      format = "%Y-%m-%d %H:%M",
      tz = "UTC"
    ),
    hour = as.integer(
      str_match(Date_Begin_Local, " ([0-9]{1,2}):")[, 2]
    ),
    month = as.integer(substr(Date_Begin_Local, 6, 7))
  )

stopifnot(nrow(aeso_raw) == 8784)
stopifnot(sum(is.na(aeso_raw$hour_utc)) == 0)
stopifnot(sum(duplicated(aeso_raw$hour_utc)) == 0)
stopifnot(sum(is.na(aeso_raw$hour)) == 0)

# -------------------------------------------------------------------------
# 3. Official wind and solar generation
# -------------------------------------------------------------------------

wind <- read_csv(wind_file, show_col_types = FALSE)
solar <- read_csv(solar_file, show_col_types = FALSE)

wind_clean <- wind %>%
  transmute(
    hour_utc = parse_aeso_time(FORECAST_DATE_GMT),
    wind_MW = ACTUAL
  )

solar_clean <- solar %>%
  transmute(
    hour_utc = parse_aeso_time(FORECAST_DATE_GMT),
    solar_MW = ACTUAL
  )

stopifnot(sum(is.na(wind_clean$hour_utc)) == 0)
stopifnot(sum(is.na(solar_clean$hour_utc)) == 0)
stopifnot(sum(duplicated(wind_clean$hour_utc)) == 0)
stopifnot(sum(duplicated(solar_clean$hour_utc)) == 0)

stopifnot(
  anti_join(
    aeso_raw %>% select(hour_utc),
    wind_clean %>% select(hour_utc),
    by = "hour_utc"
  ) %>% nrow() == 0
)

stopifnot(
  anti_join(
    aeso_raw %>% select(hour_utc),
    solar_clean %>% select(hour_utc),
    by = "hour_utc"
  ) %>% nrow() == 0
)

# -------------------------------------------------------------------------
# 4. Core hourly market dataset
# -------------------------------------------------------------------------

power_2024 <- aeso_raw %>%
  select(
    Date_Begin_GMT,
    Date_Begin_Local,
    hour_utc,
    hour,
    month,
    ACTUAL_POOL_PRICE,
    ACTUAL_AIL,
    all_of(intertie_columns)
  ) %>%
  left_join(wind_clean, by = "hour_utc") %>%
  left_join(solar_clean, by = "hour_utc") %>%
  mutate(
    net_load = ACTUAL_AIL - wind_MW - solar_MW,
    total_imports = IMPORT_BC + IMPORT_MT + IMPORT_SK,
    total_exports = EXPORT_BC + EXPORT_MT + EXPORT_SK,
    # Positive = net importing; negative = net exporting
    net_imports = total_imports - total_exports
  ) %>%
  arrange(hour_utc)

stopifnot(nrow(power_2024) == 8784)
stopifnot(!anyNA(power_2024$wind_MW))
stopifnot(!anyNA(power_2024$solar_MW))
stopifnot(!anyNA(power_2024$net_imports))

# -------------------------------------------------------------------------
# 5. Net-load ramps
# -------------------------------------------------------------------------

power_2024 <- power_2024 %>%
  mutate(
    hours_elapsed = as.numeric(
      difftime(hour_utc, lag(hour_utc), units = "hours")
    ),
    ramp_MW = if_else(
      hours_elapsed == 1,
      net_load - lag(net_load),
      NA_real_
    ),
    demand_change = if_else(
      hours_elapsed == 1,
      ACTUAL_AIL - lag(ACTUAL_AIL),
      NA_real_
    ),
    wind_change = if_else(
      hours_elapsed == 1,
      wind_MW - lag(wind_MW),
      NA_real_
    ),
    solar_change = if_else(
      hours_elapsed == 1,
      solar_MW - lag(solar_MW),
      NA_real_
    ),
    calculated_ramp =
      demand_change - wind_change - solar_change
  )

ramp_qc <- power_2024 %>%
  summarise(
    max_difference = max(
      abs(ramp_MW - calculated_ramp),
      na.rm = TRUE
    )
  )

stopifnot(ramp_qc$max_difference < 1e-6)

# -------------------------------------------------------------------------
# 6. High-price definition and core summaries
# -------------------------------------------------------------------------

price_threshold <- quantile(
  power_2024$ACTUAL_POOL_PRICE,
  probs = 0.95,
  na.rm = TRUE
)

power_2024 <- power_2024 %>%
  mutate(
    high_price = ACTUAL_POOL_PRICE >= price_threshold
  )

stopifnot(sum(power_2024$high_price, na.rm = TRUE) == 440)

ramp_analysis <- power_2024 %>%
  filter(!is.na(ramp_MW), !is.na(ACTUAL_POOL_PRICE))

correlation_results <- ramp_analysis %>%
  summarise(
    correlation_netload = cor(
      net_load,
      ACTUAL_POOL_PRICE,
      method = "spearman",
      use = "complete.obs"
    ),
    correlation_ramp = cor(
      ramp_MW,
      ACTUAL_POOL_PRICE,
      method = "spearman",
      use = "complete.obs"
    )
  )

price_spike_summary <- power_2024 %>%
  group_by(high_price) %>%
  summarise(
    n = n(),
    mean_price = mean(ACTUAL_POOL_PRICE, na.rm = TRUE),
    median_price = median(ACTUAL_POOL_PRICE, na.rm = TRUE),
    mean_demand = mean(ACTUAL_AIL, na.rm = TRUE),
    mean_net_load = mean(net_load, na.rm = TRUE),
    mean_wind = mean(wind_MW, na.rm = TRUE),
    mean_solar = mean(solar_MW, na.rm = TRUE),
    mean_ramp = mean(ramp_MW, na.rm = TRUE),
    mean_abs_ramp = mean(abs(ramp_MW), na.rm = TRUE),
    .groups = "drop"
  )

high_price_by_hour <- power_2024 %>%
  group_by(hour) %>%
  summarise(
    total_hours = n(),
    high_price_hours = sum(high_price, na.rm = TRUE),
    high_price_share = 100 * mean(high_price, na.rm = TRUE),
    .groups = "drop"
  )

high_price_by_month <- power_2024 %>%
  group_by(month) %>%
  summarise(
    total_hours = n(),
    high_price_hours = sum(high_price, na.rm = TRUE),
    high_price_share = 100 * mean(high_price, na.rm = TRUE),
    .groups = "drop"
  )

jan_july_comparison <- power_2024 %>%
  filter(month %in% c(1, 7)) %>%
  mutate(
    month_name = if_else(month == 1, "January", "July"),
    price_group = if_else(high_price, "High price", "Normal price")
  ) %>%
  group_by(month_name, price_group) %>%
  summarise(
    n = n(),
    mean_price = mean(ACTUAL_POOL_PRICE, na.rm = TRUE),
    mean_demand = mean(ACTUAL_AIL, na.rm = TRUE),
    mean_net_load = mean(net_load, na.rm = TRUE),
    mean_wind = mean(wind_MW, na.rm = TRUE),
    mean_solar = mean(solar_MW, na.rm = TRUE),
    mean_abs_ramp = mean(abs(ramp_MW), na.rm = TRUE),
    .groups = "drop"
  )

intertie_price_summary <- power_2024 %>%
  group_by(high_price) %>%
  summarise(
    n = n(),
    mean_price = mean(ACTUAL_POOL_PRICE, na.rm = TRUE),
    mean_imports = mean(total_imports, na.rm = TRUE),
    mean_exports = mean(total_exports, na.rm = TRUE),
    mean_net_imports = mean(net_imports, na.rm = TRUE),
    median_net_imports = median(net_imports, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# 7. Matched asset-level generation
# -------------------------------------------------------------------------

generation_long <- aeso_raw %>%
  select(hour_utc, all_of(asset_columns)) %>%
  pivot_longer(
    cols = all_of(asset_columns),
    names_to = "ASSET_SHORT_NAME",
    values_to = "generation_MW"
  ) %>%
  left_join(
    csd_assets %>%
      select(
        ASSET_SHORT_NAME,
        FUEL_TYPE,
        SUB_FUEL_TYPE,
        MAXIMUM_CAPABILITY
      ),
    by = "ASSET_SHORT_NAME"
  )

stopifnot(sum(is.na(generation_long$FUEL_TYPE)) == 0)

generation_by_fuel <- generation_long %>%
  group_by(hour_utc, FUEL_TYPE) %>%
  summarise(
    # Preserve NA if any asset in a fuel group is missing
    generation_MW = if (any(is.na(generation_MW))) {
      NA_real_
    } else {
      sum(generation_MW)
    },
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = FUEL_TYPE,
    values_from = generation_MW
  )

# Asset-based wind/solar are useful for QC, but official aggregate
# wind_MW and solar_MW remain the primary renewable series.
generation_check <- power_2024 %>%
  left_join(
    generation_by_fuel %>%
      select(hour_utc, WIND, SOLAR),
    by = "hour_utc"
  ) %>%
  summarise(
    wind_correlation =
      cor(wind_MW, WIND, use = "complete.obs"),
    wind_mean_difference =
      mean(WIND - wind_MW, na.rm = TRUE),
    solar_correlation =
      cor(solar_MW, SOLAR, use = "complete.obs"),
    solar_mean_difference =
      mean(SOLAR - solar_MW, na.rm = TRUE)
  )

# These gas/hydro values are from matched assets in the historical CSV,
# not necessarily total fleet generation.
power_2024 <- power_2024 %>%
  left_join(
    generation_by_fuel %>%
      select(hour_utc, GAS, HYDRO) %>%
      rename(
        gas_generation_MW = GAS,
        hydro_generation_MW = HYDRO
      ),
    by = "hour_utc"
  )

stopifnot(!anyNA(power_2024$gas_generation_MW))
stopifnot(!anyNA(power_2024$hydro_generation_MW))

generation_price_summary <- power_2024 %>%
  group_by(high_price) %>%
  summarise(
    n = n(),
    mean_price = mean(ACTUAL_POOL_PRICE, na.rm = TRUE),
    mean_gas_generation =
      mean(gas_generation_MW, na.rm = TRUE),
    mean_hydro_generation =
      mean(hydro_generation_MW, na.rm = TRUE),
    median_gas_generation =
      median(gas_generation_MW, na.rm = TRUE),
    median_hydro_generation =
      median(hydro_generation_MW, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------------------------
# 8. Saved AESO generation-capacity / outage data
# -------------------------------------------------------------------------

# The live API pull was saved once to CSV. Reading the saved file makes
# the portfolio analysis reproducible without exposing or requesting an API key.
outage_2024_long <- read_csv(
  outage_file,
  show_col_types = FALSE
) %>%
  mutate(
    hour_utc = as.POSIXct(hour_utc, tz = "UTC")
  )

stopifnot(n_distinct(outage_2024_long$hour_utc) == 8784)

outage_identity_qc <- outage_2024_long %>%
  summarise(
    max_difference = max(
      abs(MC - MBO_OUT - OP_OUT - AC),
      na.rm = TRUE
    )
  )

stopifnot(outage_identity_qc$max_difference == 0)

# Gas and hydro were present in all 8,784 hours. Coal and dual-fuel
# categories were not present in every API hour, so the full-year
# availability comparison is restricted to gas and hydro.
core_fuel_qc <- outage_2024_long %>%
  group_by(hour_utc) %>%
  summarise(
    has_gas = any(fuel_type == "GAS"),
    has_hydro = any(fuel_type == "HYDRO"),
    .groups = "drop"
  )

stopifnot(sum(!core_fuel_qc$has_gas) == 0)
stopifnot(sum(!core_fuel_qc$has_hydro) == 0)

outage_hourly_2024 <- outage_2024_long %>%
  group_by(hour_utc) %>%
  summarise(
    gas_MC = sum(MC[fuel_type == "GAS"]),
    gas_outage = sum(
      OP_OUT[fuel_type == "GAS"] +
        MBO_OUT[fuel_type == "GAS"]
    ),
    gas_AC = sum(AC[fuel_type == "GAS"]),
    hydro_MC = sum(MC[fuel_type == "HYDRO"]),
    hydro_outage = sum(
      OP_OUT[fuel_type == "HYDRO"] +
        MBO_OUT[fuel_type == "HYDRO"]
    ),
    hydro_AC = sum(AC[fuel_type == "HYDRO"]),
    .groups = "drop"
  ) %>%
  mutate(
    gas_outage_pct = 100 * gas_outage / gas_MC,
    hydro_outage_pct = 100 * hydro_outage / hydro_MC,
    gas_hydro_outage = gas_outage + hydro_outage,
    gas_hydro_AC = gas_AC + hydro_AC
  )

stopifnot(
  anti_join(
    power_2024 %>% select(hour_utc),
    outage_hourly_2024 %>% select(hour_utc),
    by = "hour_utc"
  ) %>% nrow() == 0
)

power_2024 <- power_2024 %>%
  left_join(outage_hourly_2024, by = "hour_utc")

# -------------------------------------------------------------------------
# 9. Final results
# -------------------------------------------------------------------------

final_outage_summary <- power_2024 %>%
  group_by(high_price) %>%
  summarise(
    n = n(),
    mean_price = mean(ACTUAL_POOL_PRICE, na.rm = TRUE),
    mean_gas_outage = mean(gas_outage, na.rm = TRUE),
    median_gas_outage = median(gas_outage, na.rm = TRUE),
    mean_gas_outage_pct = mean(gas_outage_pct, na.rm = TRUE),
    mean_gas_AC = mean(gas_AC, na.rm = TRUE),
    mean_hydro_outage = mean(hydro_outage, na.rm = TRUE),
    mean_gas_hydro_outage =
      mean(gas_hydro_outage, na.rm = TRUE),
    mean_gas_hydro_AC =
      mean(gas_hydro_AC, na.rm = TRUE),
    .groups = "drop"
  )

final_market_summary <- power_2024 %>%
  group_by(high_price) %>%
  summarise(
    n = n(),
    mean_price = mean(ACTUAL_POOL_PRICE, na.rm = TRUE),
    mean_demand = mean(ACTUAL_AIL, na.rm = TRUE),
    mean_net_load = mean(net_load, na.rm = TRUE),
    mean_wind = mean(wind_MW, na.rm = TRUE),
    mean_solar = mean(solar_MW, na.rm = TRUE),
    mean_ramp = mean(ramp_MW, na.rm = TRUE),
    mean_net_imports = mean(net_imports, na.rm = TRUE),
    mean_gas_generation =
      mean(gas_generation_MW, na.rm = TRUE),
    mean_hydro_generation =
      mean(hydro_generation_MW, na.rm = TRUE),
    mean_gas_outage = mean(gas_outage, na.rm = TRUE),
    mean_gas_outage_pct =
      mean(gas_outage_pct, na.rm = TRUE),
    mean_gas_AC = mean(gas_AC, na.rm = TRUE),
    .groups = "drop"
  )

print(correlation_results)
print(price_spike_summary, width = Inf)
print(intertie_price_summary, width = Inf)
print(generation_price_summary, width = Inf)
print(final_outage_summary, width = Inf)
print(final_market_summary, width = Inf)

# -------------------------------------------------------------------------
# 10. Final portfolio figures
# -------------------------------------------------------------------------

# Figure 1: Pool price vs net load
p1 <- ggplot(
  power_2024,
  aes(x = net_load, y = ACTUAL_POOL_PRICE)
) +
  geom_point(alpha = 0.20) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Alberta Pool Price Increases with Net Load",
    subtitle = "Hourly AESO data, 2024",
    x = "Net load (MW)",
    y = "Pool price ($/MWh)"
  ) +
  theme_minimal()

ggsave(
  "figures/01_pool_price_vs_net_load.png",
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure 2: High-price frequency by hour
p2 <- ggplot(
  high_price_by_hour,
  aes(x = hour, y = high_price_share)
) +
  geom_col() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "High-Price Hours Concentrate in the Late Afternoon and Evening",
    subtitle = "Share of hours in the top 5% of pool prices, by hour of day",
    x = "Hour of day",
    y = "High-price hours (%)"
  ) +
  theme_minimal()

ggsave(
  "figures/02_high_price_frequency_by_hour.png",
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure 3: Normal vs high-price system conditions
condition_summary <- power_2024 %>%
  group_by(high_price) %>%
  summarise(
    mean_net_load = mean(net_load, na.rm = TRUE),
    mean_wind = mean(wind_MW, na.rm = TRUE),
    mean_gas_generation =
      mean(gas_generation_MW, na.rm = TRUE),
    mean_hydro_generation =
      mean(hydro_generation_MW, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    price_group =
      if_else(high_price, "High price", "Normal price")
  )

baseline <- condition_summary %>%
  filter(price_group == "Normal price") %>%
  select(
    mean_net_load,
    mean_wind,
    mean_gas_generation,
    mean_hydro_generation
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "baseline_value"
  )

condition_plot_data <- condition_summary %>%
  select(
    price_group,
    mean_net_load,
    mean_wind,
    mean_gas_generation,
    mean_hydro_generation
  ) %>%
  pivot_longer(
    cols = -price_group,
    names_to = "metric",
    values_to = "value"
  ) %>%
  left_join(baseline, by = "metric") %>%
  mutate(
    index_value = 100 * value / baseline_value,
    metric = recode(
      metric,
      mean_net_load = "Net load",
      mean_wind = "Wind generation",
      mean_gas_generation = "Gas generation",
      mean_hydro_generation = "Hydro generation"
    )
  )

p3 <- ggplot(
  condition_plot_data,
  aes(x = metric, y = index_value, fill = price_group)
) +
  geom_col(position = "dodge") +
  labs(
    title = "High-Price Hours Reflect a Much Tighter System",
    subtitle = "Normal-price hours indexed to 100",
    x = "",
    y = "Index (Normal-price hours = 100)",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave(
  "figures/03_normal_vs_high_price_conditions.png",
  plot = p3,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure 4: January vs July comparison
jan_july_plot_data <- power_2024 %>%
  filter(month %in% c(1, 7)) %>%
  mutate(
    month_name = if_else(month == 1, "January", "July"),
    price_group = if_else(high_price, "High price", "Normal price")
  ) %>%
  group_by(month_name, price_group) %>%
  summarise(
    mean_net_load = mean(net_load, na.rm = TRUE),
    mean_wind = mean(wind_MW, na.rm = TRUE),
    mean_gas_generation =
      mean(gas_generation_MW, na.rm = TRUE),
    mean_hydro_generation =
      mean(hydro_generation_MW, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      mean_net_load = "Net load",
      mean_wind = "Wind generation",
      mean_gas_generation = "Gas generation",
      mean_hydro_generation = "Hydro generation"
    )
  )

p4 <- ggplot(
  jan_july_plot_data,
  aes(x = month_name, y = value, fill = price_group)
) +
  geom_col(position = "dodge") +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title = "January and July Reach High Prices Through Similar Tight-System Conditions",
    subtitle = "Comparison of normal vs high-price hours",
    x = "",
    y = "MW",
    fill = ""
  ) +
  theme_minimal()

ggsave(
  "figures/04_january_vs_july_comparison.png",
  plot = p4,
  width = 11,
  height = 8,
  dpi = 300
)
