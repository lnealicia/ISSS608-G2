
pacman::p_load(shiny, tidytext, readtext, shinyjs, tidyverse, jsonlite, igraph, tidygraph, ggraph, visNetwork, clock, graphlayouts,plotly,ggiraph, shinyWidgets, ggtext)

source("helpers/Settings.R", local = TRUE)$value



# Data loading
data <- reactive({
  nodes <- fromJSON("data/cleaned_nodes.rds")
  links <- fromJSON("data/cleaned_links.rds")
  
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
    corpGraphUI(supernetwork, "corpGraph"),
    networkGraphUI(supernetwork, "networkGraph"),
    influenceGraphUI(supernetwork, "influenceGraph"),
  )
)

server <- function(input, output, session) {
  moduleServer("corpGraph", corpGraphServer, session = session)
  moduleServer("networkGraph", networkGraphServer, session = session)
  moduleServer("influenceGraph", influenceGraphServer, session = session)
}

shinyApp(ui = ui, server = server)