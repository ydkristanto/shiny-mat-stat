library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)

# UI ----
ui <- page_navbar(
    title = "Reproducible Example",
    sidebar = sidebar(
        "Sidebar"
    ),
    nav_panel(
        title = "Page 1",
        leafletOutput("map")
    )
)

# Server ----
server <- function(input, output) {
    output$map <- renderLeaflet({
        m <- leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
        m  # Print the map
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
