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
    City = trimws(City),
    Name = trimws(Name),
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
  
  c(lo = mu - k * s, hi = mu + k * s)
}

clamp_vals <- function(x, lo, hi) pmin(pmax(x, lo), hi)

# -----------------------
# UI
# -----------------------
ui <- fluidPage(
  titlePanel("Neighborhood Home Prices"),
  
  fluidRow(
    column(
      width = 1,
      helpText(" ")
    ),
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
      width = 5,
      helpText("Use the layer control on the map to toggle variables.")
    )
  ),
  
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
  
  # store active base layer
  active_base <- reactiveVal("2-year % change")
  
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
    
    nbds_join %>%
      filter(State == input$state, City == input$city)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addControl(
        html = HTML("
          Data <a href='https://www.zillow.com/research/data/' target='_blank'>Zillow Home Value Index</a> •
          <a href='https://catalog.data.gov/dataset/zillow-neighborhood-boundaries' target='_blank'>Zillow Neighborhood Boundaries</a> •
          Map Shuler Hopkins
        "),
        position = "bottomleft"
      ) %>%
      addLayersControl(
        baseGroups = c("2-year % change", "Current price"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;

          if (window.Shiny) {
            Shiny.setInputValue('map_ready', Date.now(), {priority: 'event'});
            Shiny.setInputValue('active_base', '2-year % change', {priority: 'event'});
          }

          map.on('baselayerchange', function(e) {
            if (window.Shiny) {
              Shiny.setInputValue('active_base', e.name, {priority: 'event'});
            }
          });
        }
      ")
  })
  
  observeEvent(input$active_base, {
    req(input$active_base)
    active_base(input$active_base)
  }, ignoreInit = TRUE)
  
  observeEvent(list(input$map_ready, input$state, input$city, active_base()), {
    req(input$map_ready)
    
    df <- city_sf()
    req(nrow(df) > 0)
    
    df <- st_transform(df, 4326)
    
    lim_change <- clamp_limits(df$prev_2y_perc, k = 2)
    lo <- lim_change[["lo"]]
    hi <- lim_change[["hi"]]
    
    lim_current <- clamp_limits(df$current, k = 2)
    lo2 <- lim_current[["lo"]]
    hi2 <- lim_current[["hi"]]
    
    pal_change  <- colorNumeric("RdBu", domain = c(lo, hi), na.color = "#f0f0f0")
    pal_current <- colorNumeric("RdBu", domain = c(lo2, hi2), na.color = "#f0f0f0")
    
    pops <- sprintf(
      "%s<br/>Current: %s<br/>2-year change: %s",
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
      removeControl("main_legend")
    
    proxy <- proxy %>%
      addPolygons(
        group = "2-year % change",
        fillColor = ~pal_change(clamp_vals(prev_2y_perc, lo, hi)),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = pops,
        highlightOptions = highlightOptions(
          weight = 2, color = "#333", fillOpacity = 0.9, bringToFront = TRUE
        )
      ) %>%
      addPolygons(
        group = "Current price",
        fillColor = ~pal_current(clamp_vals(current, lo2, hi2)),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = pops,
        highlightOptions = highlightOptions(
          weight = 2, color = "#333", fillOpacity = 0.9, bringToFront = TRUE
        )
      )
    
    if (active_base() == "Current price") {
      proxy <- proxy %>%
        showGroup("Current price") %>%
        hideGroup("2-year % change") %>%
        addLegend(
          position = "bottomright",
          pal = pal_current,
          values = c(lo2, hi2),
          title = "Current price (ZHVI)",
          opacity = 1,
          layerId = "main_legend",
          labFormat = labelFormat(
            prefix = "$",
            big.mark = ",",
            digits = 0
          )
        )
    } else {
      proxy <- proxy %>%
        showGroup("2-year % change") %>%
        hideGroup("Current price") %>%
        addLegend(
          position = "bottomright",
          pal = pal_change,
          values = c(lo, hi),
          title = "2-year % change",
          opacity = 1,
          layerId = "main_legend",
          labFormat = labelFormat(
            transform = function(x) 100 * x,
            suffix = "%"
          )
        )
    }
    
    proxy %>%
      fitBounds(xmin, ymin, xmax, ymax)
    
  }, ignoreInit = FALSE)
}

shinyApp(ui, server)

# fish
