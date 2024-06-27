# Load required libraries
pacman::p_load(
  shiny, tidytext, readtext, shinyjs, tidyverse, jsonlite, 
  igraph, tidygraph, ggraph, visNetwork, clock, graphlayouts,
  plotly, ggiraph, shinyWidgets, ggtext
)

# Source helper settings
source("helpers/Settings.R", local = TRUE)$value

kkgraphUI <- source("ui/corp_structure.R", local = TRUE)$value
kkgraphServer <- source("server/corp_structure.R", local = TRUE)$value

ytgraphUI <- source("ui/network_graph.R", local = TRUE)$value
ytgraphServer <- source("server/network_graph.R", local = TRUE)$value
stylesUI <- source("ui/styles.R", local = TRUE)$value
aligraphUI <- source("ui/influence_graph.R", local = TRUE)$value
aligraphServer <- source("server/influence_graph.R", local = TRUE)$value


# Data loading
data <- reactive({
  nodes <- readRDS("data/rds/cleaned_nodes.rds")
  links <- readRDS("data/rds/cleaned_links.rds")
  
  # Merge nodes and links data
  merged_links <- links %>%
    left_join(nodes, by = c("source" = "id")) %>%
    left_join(nodes, by = c("target" = "id"), suffix = c(".source", ".target"))
  
  supernetwork <- list(nodes = nodes, links = merged_links, vip_connections = vip_connections)
  return(supernetwork)
})

# Define UI
ui <- tagList(
  useShinyjs(),
  navbarPage(
    title = "The Red Herring Hunt",
    fluid = TRUE,
    inverse = TRUE,
    collapsible = TRUE,
    tabPanel("VIP Connections", graphUI("vipGraph"))
  )
)

# Define server logic
server <- function(input, output, session) {
  callModule(graphServer, "vipGraph", data)
}

# Run the application
shinyApp(ui = ui, server = server)
