library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(scales)
options(tigris_use_cache = TRUE)

# map data
tn_counties <- counties(state = "TN", cb = TRUE, year = 2023) %>%  # cb=cartographic boundaries (lighter)
  st_as_sf()

# Zillow home value index data
zhvi_county_raw <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1772201941")

# Set dates
present_date <- date("2026-01-31")

prev_6m <- present_date - months(6)
prev_1y <- present_date - years(1)
prev_2y <- present_date - years(2)
prev_5y <- present_date - years(5)
prev_10y <- present_date - years(10)

# clean data
zhvi_tn_clean <- zhvi_county_raw |>
  filter(StateName == "TN")|>
  mutate(
    prev_6m = .data[[as.character(present_date)]] - .data[[as.character(prev_6m)]],
    prev_1y = .data[[as.character(present_date)]] - .data[[as.character(prev_1y)]],
    prev_2y = .data[[as.character(present_date)]] - .data[[as.character(prev_2y)]],
    prev_5y = .data[[as.character(present_date)]] - .data[[as.character(prev_5y)]],
    prev_10y = .data[[as.character(present_date)]] - .data[[as.character(prev_10y)]],
    prev_6m_perc = .data[[as.character(present_date)]] / .data[[as.character(prev_6m)]] - 1,
    prev_1y_perc = .data[[as.character(present_date)]] / .data[[as.character(prev_1y)]] -1,
    prev_2y_perc = .data[[as.character(present_date)]] / .data[[as.character(prev_2y)]] -1,
    prev_5y_perc = .data[[as.character(present_date)]] / .data[[as.character(prev_5y)]] -1,
    prev_10y_perc = .data[[as.character(present_date)]] / .data[[as.character(prev_10y)]] -1,
  )|>
  select(RegionName, `2026-01-31`:prev_10y_perc)|>
  rename(NAME = RegionName)|>
  mutate(NAME = str_remove(NAME, " County$"))

# Join data to map
tn_counties_join <- tn_counties|>
  left_join(zhvi_tn_clean, by = "NAME")

# Create plot
vis <- ggplot(tn_counties_join) +
  geom_sf(aes(fill = prev_2y_perc), color = "white", linewidth = 0.15) +
  scale_fill_gradient2(
    midpoint = 0,
    oob = squish,                 # squish outliers instead of letting them dominate
    name = NULL,
    breaks = c(-.05,0,.05,.10,.15),
    labels = label_percent()
  ) +
  labs(
    title = "Tennessee Home Price Change by County",
    subtitle = "Percent change over the last 2 years",
    caption = paste(
      "Home values: Zillow Research (ZHVI).",
      "County boundaries: US Census TIGER/Line via tigris.",
      "Analysis: Shuler Hopkins."
    )
  ) +
  theme_void()+
  theme(
    plot.margin = margin(t = 5, r = 15, b = 5, l = 15),
    legend.position = "bottom",
    legend.key.width = unit(3, "cm"),
    legend.key.height = unit(0.5, "cm"),
    plot.caption = element_text(size = 8, color = "grey40")
  )
vis

ggsave("outputs/tn_county_map_2y.jpeg", plot = vis, width = 8, height = 5, dpi = 300)

