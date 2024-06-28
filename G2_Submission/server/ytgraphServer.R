ytgraphServer <- function(input, output, session, nodes, links) {
  # Observe the starting alphabet and update the VIP choices accordingly
  observeEvent(input$startingAlphabet, {
    selected_alphabet <- input$startingAlphabet
    
    # Filter nodes based on the selected starting alphabet
    if (selected_alphabet == "0-9") {
      filtered_nodes <- nodes() %>%
        filter(grepl("^[0-9]", id)) %>%
        select(id)
    } else {
      filtered_nodes <- nodes() %>%
        filter(grepl(paste0("^", selected_alphabet), id, ignore.case = TRUE)) %>%
        select(id)
    }
    
    updateSelectizeInput(session, "selectedVIP", choices = filtered_nodes$id)
  })
  
  # Render the network plot based on selected VIP
  output$networkPlot <- renderVisNetwork({
    req(input$selectedVIP)
    
    selected <- input$selectedVIP
    selected_node <- nodes() %>% filter(id == selected)
    
    # Filter the links to show only those related to the selected VIP
    filtered_links <- links() %>% 
      filter(source == selected_node$id | target == selected_node$id)
    
    # Filter the nodes to include only those connected by the filtered links
    filtered_node_ids <- unique(c(filtered_links$source, filtered_links$target))
    filtered_nodes <- nodes() %>% 
      filter(id %in% filtered_node_ids)
    
    # Customize node colors
    filtered_nodes <- filtered_nodes %>% 
      mutate(color.background = ifelse(id == selected, "red", "lightblue"),
             color.border = ifelse(id == selected, "darkred", "darkblue"),
             font.color = ifelse(id == selected, "white", "black"))
    
    # Create the igraph object
    g <- graph_from_data_frame(filtered_links, vertices = filtered_nodes, directed = TRUE)
    
    # Calculate degree centrality
    degree_centrality <- degree(g, mode = "all")
    
    # Add centrality scores to the nodes
    filtered_nodes <- filtered_nodes %>%
      mutate(centrality = degree_centrality[match(id, names(degree_centrality))])
    
    # Customize node labels to include centrality scores
    filtered_nodes <- filtered_nodes %>%
      mutate(label_display = paste0(id, "\nCentrality: ", round(centrality, 2)))
    
    # Convert to data frames for visNetwork
    nodes_df <- filtered_nodes %>% select(id, label = label_display, color.background, color.border, font.color)
    links_df <- filtered_links %>% select(from = source, to = target)
    
    # Plot the network with customized nodes and layout
    visNetwork(nodes_df, links_df) %>%
      visNodes(color = list(
        background = nodes_df$color.background, 
        border = nodes_df$color.border
      ),
      font = list(color = nodes_df$font.color)) %>%
      visEdges(arrows = 'to') %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123)  # Ensures consistent layout
  })
}
