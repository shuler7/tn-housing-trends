library(tidyverse)
# Zillow Home Value Index (ZHVI)

# state data
zhvi_state <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/State_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1772201941")

# county data
zhvi_county <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1772201941")

# city data (large)
zhvi_city <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/City_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1772201941")

# zip code data (very large)
zhvi_zip <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1772201941")

# neighborhood data (very large)
zhvi_nbd <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Neighborhood_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1772201941")
