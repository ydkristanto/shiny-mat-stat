library(shiny)
library(bslib)
library(tidyverse)

# UI ----
ui <- page_navbar(
    title = "Reproducible Example",
    sidebar = sidebar(
        "Sidebar"
    ),
    nav_panel(
        title = "Page 1",
        card(
            card_header("Card Header"),
            layout_columns(
                card_body(
                    card_title("Title 1", container = htmltools::h5),
                    tags$iframe(
                        src="https://www.youtube.com/embed/5vheNbQlsyU?si=Z1RSuFsljrWbDTZ1",
                        width="100%",
                        height="315",
                        title="YouTube video player",
                        frameborder="0"
                    )
                ),
                card_body(
                    card_title("Title 2", container = htmltools::h5),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean hendrerit faucibus dolor sed facilisis. Quisque in urna vehicula, congue enim non, hendrerit tortor. Suspendisse at ultrices purus.", style = "text-align: justify; hyphens: auto;")
                ),
                card_body(
                    card_title("Title 3", container = htmltools::h5),
                    p("Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Donec a erat id nunc consectetur scelerisque a eu turpis. Suspendisse potenti. Donec et sagittis ligula, et malesuada risus.", style = "text-align: justify; hyphens: auto;")
                ),
                col_widths = breakpoints(
                    sm = c(12, 12, 12),
                    md = c(12, 6, 6),
                    lg = c(6, 3, 3)
                )
            ),
            fill = FALSE
        )
    )
)

# Server ----
server <- function(input, output) {
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
