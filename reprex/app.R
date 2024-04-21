library(shiny)
library(plotly)
if (FALSE) {
    library(igraph)
}

# Data ----
actors <- data.frame(
    name = 1:5,
    label = c("Alice", "Bob", "Cecil", "David", "Esmeralda"),
    age = c(48, 33, 45, 34, 21),
    gender = c("F", "M", "F", "M", "F")
)
relations <- data.frame(
    from = c(2, 3, 3, 4, 4, 5),
    to = c(1, 2, 1, 1, 2, 1),
    same.dept = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
    friendship = c(4, 5, 5, 2, 1, 1), advice = c(4, 5, 5, 4, 2, 3)
)
G <- graph_from_data_frame(
    relations, directed = FALSE, vertices = actors
)
L <- layout_nicely(G)

# UI ----
ui <- fluidPage(
    titlePanel("Reprex: Network Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "nodes",
                "Nodes:",
                c(letters[1:4]),
                selected = letters[2:4],
                multiple = TRUE,
                selectize = TRUE
            )
        ),
        mainPanel(
           plotlyOutput("plot")
        )
    )
)

# Server ----
server <- function(input, output) {
    output$plot <- renderPlotly({
        ## Create Vertices and Edges ----
        vs <- V(G)
        es <- as.data.frame(get.edgelist(G))
        
        Nv <- length(vs)
        Ne <- length(es[1]$V1)
        
        ## Create Nodes ----
        Xn <- L[,1]
        Yn <- L[,2]
        
        network <- plot_ly(
            x = ~Xn, y = ~Yn, mode = "markers",
            text = vs$label, hoverinfo = "text"
        )
        
        ## Creates Edges ----
        edge_shapes <- list()
        for(i in 1:Ne) {
            v0 <- es[i,]$V1 |>
                as.numeric()
            v1 <- es[i,]$V2 |>
                as.numeric()
            
            edge_shape = list(
                type = "line",
                line = list(color = "#030303", width = 0.3),
                x0 = Xn[v0],
                y0 = Yn[v0],
                x1 = Xn[v1],
                y1 = Yn[v1]
            )
            
            edge_shapes[[i]] <- edge_shape
        }
        
        ## Create Network ----
        axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
        
        fig <- layout(
            network,
            title = 'Karate Network',
            shapes = edge_shapes,
            xaxis = axis,
            yaxis = axis
        )
        
        fig
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
