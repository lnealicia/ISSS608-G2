network_server <- function(input, output, session, nodes, links, selected_companies) {
  ns <- session$ns
  
  # Reactive expression to generate the graph data
  graph_data <- reactive({
    req(input$year)
    year <- input$year
    
    # Load nodes and links data
    nodes_data <- nodes()
    links_data <- links()
    
    # Debugging statements
    print(head(nodes_data))
    print(head(links_data))
    
    # Ensure that the 'name' column exists in nodes_data
    if (!"id" %in% colnames(nodes_data)) {
      stop("Column 'id' not found in nodes data")
    }
    
    # Filter nodes to include only selected companies
    nodes_filtered <- nodes_data %>% filter(id %in% selected_companies)
    
    # Ensure that the 'source' and 'target' columns exist in links_data
    if (!all(c("source", "target") %in% colnames(links_data))) {
      stop("Columns 'source' and 'target' not found in links data")
    }
    
    # Filter edges based on selected year and selected companies
    edges_filtered <- links_data %>%
      filter((source %in% nodes_filtered$id & target %in% nodes_filtered$id) &
               year(start_date) <= year & (is.na(end_date) | year(end_date) >= year))
    
    # Debugging statements
    print(head(edges_filtered))
    
    # Create igraph object
    graph <- graph_from_data_frame(d = edges_filtered, vertices = nodes_filtered, directed = TRUE)
    graph
  })
  
  # Render the graph plot
  output$graphPlot <- renderPlot({
    ggraph(graph_data(), layout = "fr") +  # Using Fruchterman-Reingold layout
      geom_edge_link(aes(label = as.character(year(start_date))),  # Only label with start_date year
                     arrow = arrow(length = unit(4, 'mm')),  # Add arrows to indicate direction
                     end_cap = circle(3, 'mm'),  # Cap the end of the edges with a circle
                     label_dodge = unit(2, "mm"),  # Adjust label position to avoid overlap
                     label_size = 3,  # Set label size
                     edge_width = 0.8,  # Set edge width
                     edge_alpha = 0.8) +  # Set edge transparency
      geom_node_point(size = 5, color = "blue") +  # Customize node appearance
      geom_node_text(aes(label = name), vjust = 1.5, size = 4) +  # Add node labels
      theme_void() +  # Use a void theme
      labs(title = "Directed Network Graph of Key Personnel Transactions",
           subtitle = "Nodes represent unique sources and targets, edges labeled with year",
           caption = "Data Source: keypersonnel")  # Add titles and captions
  })
}
