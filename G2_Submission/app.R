
pacman::p_load(shiny, tidytext, readtext, shinyjs, tidyverse, jsonlite, igraph, tidygraph, ggraph, visNetwork, clock, graphlayouts,plotly,ggiraph, shinyWidgets, ggtext)

source("helpers/Settings.R", local = TRUE)$value

# Data loading
data <- reactive({
  nodes <- readRDS("data/rds/cleaned_nodes.rds")
  links <- readRDS("data/rds/cleaned_links.rds")
  
  supernetwork <- list(nodes = nodes, links = links)
  return(supernetwork)
})

# Define UI

ui <- tagList(
  useShinyjs(),
  stylesUI,
  navbarPage(
    title = "The Red Herring Hunt",
    fluid = TRUE,
    inverse = TRUE,
    collapsible = TRUE,
    kkgraphUI(supernetwork, "kkgraph"),
    ytgraphUI(supernetwork, "ytgraph"),
    aligraphUI(supernetwork, "aligraph")
  )
)

server <- function(input, output, session) {
  moduleServer("kkgraph", kkgraphServer, session = session)
  moduleServer("ytgraph", ytgraphServer, session = session)
  moduleServer("aligraph", aligraphServer, session = session)
}

shinyApp(ui = ui, server = server)
