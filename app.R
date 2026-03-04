library(shiny)
library(dplyr)
library(lubridate)
library(sf)
library(leaflet)
library(scales)
library(htmltools)
library(htmlwidgets)

# ---- load data ----
nbds_join <- readRDS("data/nbds_join.rds")

# Basic cleanup / ensure numeric
nbds_join <- nbds_join %>%
  mutate(
    State = trimws(State),
    City  = trimws(City),
    Name  = trimws(Name),
    current = as.numeric(current),
    prev_2y_perc = as.numeric(prev_2y_perc)
  )

# Helpers
clamp_limits <- function(x, k = 2) {
  mu <- mean(x, na.rm = TRUE)
  s  <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) {
    r <- range(x, na.rm = TRUE)
    return(c(lo = r[1], hi = r[2]))
  }
  c(lo = mu - k*s, hi = mu + k*s)
}
clamp_vals <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# -----------------------
# UI
# -----------------------
ui <- fluidPage(
  titlePanel("Neighborhood Home Prices"),
  
  # --- TOP BAR WITH INPUTS ---
  fluidRow(
    column(
      width = 3,
      selectInput(
        "state", "State",
        choices = sort(unique(nbds_join$State)),
        selected = if ("TN" %in% unique(nbds_join$State)) "TN" else sort(unique(nbds_join$State))[1]
      )
    ),
    column(
      width = 3,
      uiOutput("city_ui")
    ),
    column(
      width = 6,
      helpText("Use the layer control on the map to toggle variables.")
    )
  ),
  
  # --- MAP ROW ---
  fluidRow(
    column(
      width = 12,
      leafletOutput("map", height = "80vh")
    )
  )
)

# -----------------------
# SERVER
# -----------------------
server <- function(input, output, session) {
  
  output$city_ui <- renderUI({
    req(input$state)
    
    cities <- nbds_join %>%
      st_drop_geometry() %>%
      filter(State == input$state) %>%
      distinct(City) %>%
      arrange(City) %>%
      pull(City)
    
    default_city <- if ("Chattanooga" %in% cities) "Chattanooga" else cities[1]
    selectInput("city", "City", choices = cities, selected = default_city)
  })
  
  city_sf <- reactive({
    req(input$state, input$city)
    nbds_join %>% filter(State == input$state, City == input$city)
  })
  
  # Initial render: tiles + layer control + CSS + JS (no shapes yet)
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      # --- attribution bar (bottom-left) ---
      addControl(
        html = HTML("
          <div class='nyt-attrib'>
            <span class='nyt-attrib-label'>Data</span>
            <a href='https://www.zillow.com/research/data/' target='_blank' rel='noopener'>
              Zillow Home Value Index
            </a>
            <span class='nyt-attrib-sep'>•</span>
            <a href='https://catalog.data.gov/dataset/neighborhoods-us-2017-zillow-segs10' target='_blank' rel='noopener'>
              Zillow Neighborhood Boundaries
            </a>
            <span class='nyt-attrib-sep'>•</span>
            <span class='nyt-attrib-label'>Map</span>
            <span>Shuler Hopkins</span>
          </div>
      
          <style>
            /* Container: slim, newspaper-ish */
            .nyt-attrib{
              font: 11px/1.25 -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
              color: rgba(0,0,0,0.72);
              background: rgba(255,255,255,0.82);
              border: 1px solid rgba(0,0,0,0.08);
              border-radius: 6px;
              padding: 6px 10px;
              box-shadow: 0 2px 10px rgba(0,0,0,0.08);
              backdrop-filter: blur(6px);
              -webkit-backdrop-filter: blur(6px);
              white-space: nowrap;
            }
      
            /* Small caps-like labels */
            .nyt-attrib-label{
              letter-spacing: 0.06em;
              text-transform: uppercase;
              font-size: 10px;
              color: rgba(0,0,0,0.55);
              margin-right: 6px;
            }
      
            .nyt-attrib-sep{
              margin: 0 8px;
              color: rgba(0,0,0,0.35);
            }
      
            .nyt-attrib a{
              color: rgba(0,0,0,0.72);
              text-decoration: none;
              border-bottom: 1px solid rgba(0,0,0,0.25);
              padding-bottom: 1px;
            }
      
            .nyt-attrib a:hover{
              color: rgba(0,0,0,0.90);
              border-bottom-color: rgba(0,0,0,0.55);
            }
      
            /* Make sure Leaflet doesn't clamp widths */
            .leaflet-control .nyt-attrib{
              max-width: none !important;
            }
          </style>
        "),
        position = "bottomleft"
      ) %>%
     
      addLayersControl(
        baseGroups = c("2-year % change", "Current price"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
      # Legend styling + hide current legend by default
      addControl(
        html = HTML("
          <style>
            .legend-change, .legend-current {
              background: rgba(255,255,255,0.9);
              padding: 6px 8px;
              border-radius: 4px;
              box-shadow: 0 0 15px rgba(0,0,0,0.2);
              line-height: 18px;
            }
          </style>
        "),
        position = "topleft"
      ) %>%
    
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
      
          function setDisplay(nodeList, display) {
            if (!nodeList) return;
            for (var i = 0; i < nodeList.length; i++) {
              nodeList[i].style.display = display;
            }
          }
      
          function showLegend(which) {
            // Grab ALL matching legends, not just the first one
            var lcs = el.querySelectorAll('.legend-change');
            var lps = el.querySelectorAll('.legend-current');
      
            if (which === 'change') {
              setDisplay(lcs, 'block');
              setDisplay(lps, 'none');
            } else if (which === 'current') {
              setDisplay(lcs, 'none');
              setDisplay(lps, 'block');
            }
          }
      
          // default
          showLegend('change');
      
          // Try again shortly after (in case legends are inserted later via leafletProxy)
          setTimeout(function() { showLegend('change'); }, 0);
          setTimeout(function() { showLegend('change'); }, 50);
          setTimeout(function() { showLegend('change'); }, 250);
      
          map.on('baselayerchange', function(e) {
            if (e.name === '2-year % change') showLegend('change');
            if (e.name === 'Current price')  showLegend('current');
          });
      
          if (window.Shiny) {
            Shiny.setInputValue('map_ready', Date.now(), {priority: 'event'});
          }
        }
      ")
  })
  
  # Update shapes + legends + zoom when state/city changes (and after map is ready)
  observeEvent(list(input$map_ready, input$state, input$city), {
    req(input$map_ready)
    
    df <- city_sf()
    req(nrow(df) > 0)
    
    # If any city ever fails bbox/zoom, uncomment:
    # df <- st_make_valid(df)
    
    df <- st_transform(df, 4326)
    
    # Clamp limits per-city (extract as UNNAMED numerics)
    lim_change <- clamp_limits(df$prev_2y_perc, k = 2)
    lo  <- lim_change[["lo"]]
    hi  <- lim_change[["hi"]]
    
    lim_current <- clamp_limits(df$current, k = 2)
    lo2 <- lim_current[["lo"]]
    hi2 <- lim_current[["hi"]]
    
    pal_change  <- colorNumeric("RdBu",  domain = c(lo,  hi),  na.color = "#f0f0f0")
    pal_current <- colorNumeric("RdBu", domain = c(lo2, hi2), na.color = "#f0f0f0")
    
    pops <- sprintf(
      "<strong>%s</strong><br/>Current: %s<br/>2-year change: %s",
      df$Name,
      ifelse(is.na(df$current), "No data", dollar(df$current, accuracy = 1)),
      ifelse(is.na(df$prev_2y_perc), "No data", percent(df$prev_2y_perc, accuracy = 0.1))
    ) %>% lapply(HTML)
    
    bb <- st_bbox(df)
    xmin <- unname(as.numeric(bb["xmin"]))
    ymin <- unname(as.numeric(bb["ymin"]))
    xmax <- unname(as.numeric(bb["xmax"]))
    ymax <- unname(as.numeric(bb["ymax"]))
    
    proxy <- leafletProxy("map", data = df) %>%
      clearShapes() %>%
      removeControl("legend_change") %>%
      removeControl("legend_current")
    
    
    proxy <- proxy %>%
      addPolygons(
        group = "2-year % change",
        fillColor = ~pal_change(clamp_vals(prev_2y_perc, lo, hi)),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = pops,
        highlightOptions = highlightOptions(weight = 2, color = "#333", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addPolygons(
        group = "Current price",
        fillColor = ~pal_current(clamp_vals(current, lo2, hi2)),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = pops,
        highlightOptions = highlightOptions(weight = 2, color = "#333", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal_change,
        values = c(lo, hi),
        title = "2-year % change",
        opacity = 1,
        className = "legend-change",
        layerId = "legend_change",
        labFormat = labelFormat(
          transform = function(x) 100 * x,
          suffix = "%"
          )
      ) %>%
      # --- manual legend for current (explicit colors + labels) ---
      addLegend(
        position  = "bottomright",
        pal       = pal_current,
        values    = c(lo2, hi2),
        title     = "Current price (ZHVI)",
        opacity   = 1,
        className = "legend-current",
        layerId   = "legend_current",
        labFormat = labelFormat(
          prefix   = "$",
          big.mark = ",",
          digits   = 0
        )
      )
    
    proxy %>%
      fitBounds(xmin, ymin, xmax, ymax) %>%
      showGroup("2-year % change") %>%
      hideGroup("Current price")
    
  }, ignoreInit = FALSE)
}

shinyApp(ui, server)
