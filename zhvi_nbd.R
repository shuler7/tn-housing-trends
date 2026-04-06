library(tidyverse)
library(sf)
library(tigris)
library(scales)
library(leaflet)
library(htmltools)
library(htmlwidgets)
options(tigris_use_cache = TRUE)

# Choose state and city
state <- "VA"
city <- "Charlottesville"
present_date <- date("2026-01-31")

# neighborhood shape data
gdb_path <- "znbds/ZillowNeighborhoods.gdb"
st_layers(gdb_path)
nbd_geom <- st_read(gdb_path, layer = "ZillowNeighborhoods_GeoDD")

neigh_sf <- nbd_geom|>
  filter(State == state, City == city)


# neighborhood home data (very large)
zhvi_nbd <- read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Neighborhood_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1772201941")

# Get 2y date
prev_2y <- present_date - years(2)

zhvi_test <- zhvi_nbd |>
  filter(State == state, City == city)|>
  mutate(prev_2y_perc = .data[[as.character(present_date)]] / .data[[as.character(prev_2y)]] -1)|>
  select(RegionName, City, as.character(present_date), prev_2y_perc)|>
  rename(Name = RegionName,
         current = as.character(present_date))


# Join data to map
tn_nbds_join <- neigh_sf|>
  left_join(zhvi_test, by = "Name")


# Palettes for each variable
clamp_limits <- function(x, k = 2) {
  mu <- mean(x, na.rm = TRUE)
  s  <- sd(x, na.rm = TRUE)
  c(lo = mu - k*s, hi = mu + k*s)
}

clamp_vals <- function(x, lo, hi) {
  pmin(pmax(x, lo), hi)
}

  
x <- tn_nbds_join$prev_2y_perc
lims <- clamp_limits(x, k = 2)    # k sd's around mean
lo <- lims["lo"]; hi <- lims["hi"]

pal_change <- colorNumeric(
  palette = "RdBu",
  domain  = c(lo, hi),
  reverse = FALSE,
  na.color = "#f0f0f0"
)

x2 <- tn_nbds_join$current
lims2 <- clamp_limits(x2, k = 2)
lo2 <- lims2["lo"]; hi2 <- lims2["hi"]

pal_current <- colorNumeric(
  "RdBu",
  domain = c(lo2, hi2),
  na.color = "#f0f0f0")


# Popup shows BOTH variables
popups <- sprintf(
  "<strong>%s</strong><br/>
   Current: %s<br/>
   2-year change: %s",
  tn_nbds_join$Name,
  ifelse(is.na(tn_nbds_join$current), "No data", dollar(tn_nbds_join$current)),
  ifelse(is.na(tn_nbds_join$prev_2y_perc), "No data", percent(tn_nbds_join$prev_2y_perc, accuracy = 0.1))
) %>% lapply(HTML)


# create the map
m <- leaflet(tn_nbds_join) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addPolygons(
    group = "2-year % change",
    fillColor = ~pal_change(clamp_vals(prev_2y_perc, lo, hi)),
    fillOpacity = 0.5, color = "white", weight = 1,
    popup = popups
  ) %>%
  
  addPolygons(
    group = "Current price",
    fillColor = ~pal_current(clamp_vals(current, lo2, hi2)),
    fillOpacity = 0.5, color = "white", weight = 1,
    popup = popups
  ) %>%
  
  addLayersControl(
    baseGroups = c("2-year % change", "Current price"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomleft"
  ) %>%
  
  addLegend(
    position = "bottomright",
    pal = pal_change,
    values = c(lo, hi),
    title = "2-year % change",
    opacity = 1,
    className = "legend-change"
  ) %>%
  
  addLegend(
    position = "bottomright",
    pal = pal_current,
    values = c(lo2, hi2),
    title = "Current price",
    opacity = 1,
    className = "legend-current"
  ) %>%
  
  hideGroup("Current price") %>%
  
  onRender("
    function(el, x) {
      var map = this;

      function show(which) {
        var lc = el.querySelector('.legend-change');
        var lp = el.querySelector('.legend-current');
        if (lc) lc.style.display = (which === 'change') ? 'block' : 'none';
        if (lp) lp.style.display = (which === 'current') ? 'block' : 'none';
      }

      // initial
      show('change');

      map.on('baselayerchange', function(e) {
        if (e.name === '2-year % change') show('change');
        if (e.name === 'Current price') show('current');
      });
    }
  ") %>%
  addControl(
  html = htmltools::HTML("
    <style>
      .legend-change, .legend-current {
        background: rgba(255,255,255,0.5);
        padding: 6px 8px;
        border-radius: 4px;
        box-shadow: 0 0 15px rgba(0,0,0,0.2);
        line-height: 18px;
      }
    </style>
  "),
  position = "topleft"
)

m


#######################################
# In shapes but not in data
shapes_not_in_data <- anti_join(neigh_sf %>% st_drop_geometry(),
                                zhvi_test,
                                by = "Name")

# In data but not in shapes
data_not_in_shapes <- anti_join(zhvi_test,
                                neigh_sf %>% st_drop_geometry(),
                                by = "Name")
#######################################
