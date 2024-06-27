library(shiny)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(viridis)

# Load the saved variables
top_graph <- readRDS("data/top_graph.rds")

# Define server logic
server <- function(input, output) {
  # Reactive expression to filter data based on user input
  filtered_data <- reactive({
    link_type <- input$linkType
    node_types <- input$nodeType
    
    # Filter edges
    filtered_edges <- top_graph %>%
      activate(edges) %>%
      filter(type %in% link_type)
    
    # Extract and filter nodes based on edges
    node_ids <- filtered_edges %>%
      pull(from) %>%
      union(filtered_edges %>% pull(to))
    
    filtered_nodes <- top_graph %>%
      activate(nodes) %>%
      filter(id %in% node_ids & type %in% node_types)
    
    list(nodes = filtered_nodes, edges = filtered_edges)
  })
  
  output$networkPlot <- renderPlot({
    data <- filtered_data()
    
    # Create graph object
    filtered_graph <- tbl_graph(nodes = data$nodes, edges = data$edges, directed = TRUE) %>%
      mutate(betweenness_centrality = centrality_betweenness(),
             closeness_centrality = centrality_closeness())
    
    # Display the network graph
    ggraph(filtered_graph, layout = "fr") + # Using Fruchterman-Reingold layout
      geom_edge_link(aes(edge_alpha = 0.9, edge_width = 0.1)) + # Customize edge appearance
      geom_node_point(aes(size = betweenness_centrality, color = closeness_centrality)) + # Customize node appearance
      scale_color_viridis_c() + # Use viridis color scale
      theme_void() + # Use a void theme
      labs(title = "Network Graph of Business Network",
           subtitle = "Nodes colored by closeness centrality and sized by betweenness centrality",
           caption = "Data Source: mc3.json") # Add titles and captions
  })
}

