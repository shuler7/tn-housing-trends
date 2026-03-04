library(tidyverse)
library(sf)

# get raw nbd data
gdb_path <- "raw_data/ZillowNeighborhoods.gdb"
st_layers(gdb_path)
nbd_geom <- st_read(gdb_path, layer = "ZillowNeighborhoods_GeoDD")

# get ZHVI data
# ZHVI - all homes - nbd level
input_data <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Neighborhood_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1772476765")

# ZHVI - single family - nbd level
#input_data <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Neighborhood_zhvi_uc_sfr_sm_sa_month.csv?t=1772476765")

# ZHVI - 3 bedrooms  - nbd level
#input_data <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Neighborhood_zhvi_bdrmcnt_3_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1772476765")


# simplify dataset
present_date <- date("2026-01-31")
prev_2y <- present_date - years(2)
zhvi_nbd <- input_data|>
  mutate(prev_2y_perc = .data[[as.character(present_date)]] / .data[[as.character(prev_2y)]] -1)|>
  select(State, RegionName, City, as.character(present_date), prev_2y_perc)|>
  rename(Name = RegionName,
         current = as.character(present_date))

# join datasets
nbds_join <- nbd_geom|>
  left_join(zhvi_nbd, by = c("State", "City", "Name"))

# save the join
saveRDS(nbds_join, "data/nbds_join.rds")
