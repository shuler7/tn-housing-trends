library(tidyverse)
library(scales)

# city data (large)
zhvi_city <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/City_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1772201941")

city_clean <- zhvi_city |>
  filter(RegionName == "Charlottesville",
         StateName == "VA")|>
  pivot_longer(
    cols = "2000-01-31":"2026-02-28",
    names_to = "date",
    values_to = "zhvi"
  )|>
  mutate(date = as_date(date),
         month = month(date),
         year = year(date))|>
  select(date, month, year, zhvi)|>
  filter(year >= 2024)

start_val <- city_clean$zhvi[2]
end_val   <- city_clean$zhvi[nrow(city_clean)]
pct_change <- (end_val/ start_val) - 1

ggplot(city_clean, aes(x = date, y = zhvi)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = city_clean[c(1, nrow(city_clean)), ], size = 2.5) +
  scale_y_continuous(labels = label_dollar()) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "4 months") +
  labs(
    title = "Zillow Home Value Index Over the Last Two Years",
    subtitle = paste0("Percent increase: ", percent(pct_change, accuracy = 0.1)),
    x = NULL,
    y = NULL,
    caption = "Source: Zillow Home Value Index (ZHVI)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

##############################################
start_point <- plot_df %>% slice(1)
end_point   <- plot_df %>% slice(n())

pct_change <- (end_point$zhvi / start_point$zhvi) - 1

ggplot(plot_df, aes(date, zhvi)) +
  geom_line(linewidth = 1.2) +
  geom_point(data = bind_rows(start_point, end_point), size = 2.5) +
  geom_text(
    data = start_point,
    aes(label = dollar(zhvi)),
    hjust = 1.1, vjust = -0.6, size = 3.8
  ) +
  geom_text(
    data = end_point,
    aes(label = dollar(zhvi)),
    hjust = -0.1, vjust = -0.6, size = 3.8
  ) +
  scale_y_continuous(labels = label_dollar()) +
  scale_x_date(
    date_labels = "%b\n%Y",
    date_breaks = "4 months",
    expand = expansion(mult = c(0.06, 0.12))
  ) +
  labs(
    title = "Typical Home Value",
    subtitle = paste0(
      "Zillow Home Value Index, last two years  •  Change: ",
      percent(pct_change, accuracy = 0.1)
    ),
    x = NULL,
    y = NULL,
    caption = "Source: Zillow Home Value Index (ZHVI)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0)
  )

