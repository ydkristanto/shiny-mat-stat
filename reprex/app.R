library(shiny)
library(suncalc)

# UI ----
ui <- fluidPage(
    titlePanel("Reproducible Example"),
    sidebarLayout(
        sidebarPanel(
            numericInput(
                "lat",
                "Latitude:",
                value = -3.7,
                min = -90,
                max = 90
            ),
            numericInput(
                "lon",
                "Longitude:",
                value = 128.2,
                min = -180,
                max = 180
            )
        ),
        mainPanel(
           tableOutput("tabel")
        )
    )
)

# Server ----
server <- function(input, output) {
    output$tabel <- renderTable({
        dat <- getSunlightTimes(
            date = seq(
                from = Sys.Date(),
                to = as.Date("2024-12-31"),
                by = "month"
            ),
            lat = input$lat,
            lon = input$lon,
            keep = c("sunrise", "sunset")
        )
        
        dat
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
