
pacman::p_load(shiny, tidytext, readtext, shinyjs, tidyverse, jsonlite, igraph, tidygraph, ggraph, visNetwork, clock, graphlayouts,plotly,ggiraph, shinyWidgets, ggtext)

source("helpers/Settings.R", local = TRUE)$value



# Data loading
data <- reactive({
  nodes <- fromJSON("data/cleaned_nodes.rds")
  links <- fromJSON("data/cleaned_links.rds")
  
  supernetwork <- list(nodes = nodes, links = links)
  
  return(supernetwork)
})

merged_data <- reactive({
  data_list <- data()
  nodes <- data_list$nodes
  links <- data_list$links
  
  # Assuming you have a common key to merge on
  # For example, if `source` and `target` in links correspond to `id` in nodes
  merged <- links %>%
    left_join(nodes, by = c("source" = "id")) %>%
    left_join(nodes, by = c("target" = "id"), suffix = c(".source", ".target"))
  
  return(merged)
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
    corpGraphUI(merged_data, "corpGraph"),
    networkGraphUI(merged_data, "networkGraph"),
    influenceGraphUI(merged_data, "influenceGraph"),
  )
)

server <- function(input, output, session) {
  moduleServer("corpGraph", corpGraphServer, session = session)
  moduleServer("networkGraph", networkGraphServer, session = session)
  moduleServer("influenceGraph", influenceGraphServer, session = session)
}

shinyApp(ui = ui, server = server)