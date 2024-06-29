# Server logic for Atypical Transactions
kkgraphServer <- function(input, output, session, nodes, links) {
  # Preprocessing for atypical transactions
  processed_data <- reactive({
    atypical_edges <- links() %>%
      filter(type %in% c("Event.Owns.Shareholdership", "Event.WorksFor", "Event.Owns.BeneficialOwnership"))
    
    atypical_nodes <- nodes() %>%
      filter(id %in% unique(c(atypical_edges$source, atypical_edges$target)))
    
    atypical_graph <- tbl_graph(nodes = atypical_nodes, edges = atypical_edges, directed = FALSE) %>%
      mutate(betweenness_centrality = centrality_betweenness(),
             closeness_centrality = centrality_closeness())
    
    betweenness_threshold <- quantile(atypical_graph %>% activate(nodes) %>% pull(betweenness_centrality), 0.995)
    closeness_threshold <- quantile(atypical_graph %>% activate(nodes) %>% pull(closeness_centrality), 0.995)
    
    filtered_graph <- atypical_graph %>%
      activate(nodes) %>%
      filter((betweenness_centrality >= betweenness_threshold | closeness_centrality >= closeness_threshold) & betweenness_centrality > 0)
    
    top_50_links <- filtered_graph %>%
      activate(edges) %>%
      mutate(edge_betweenness_sum = .N()$betweenness_centrality[from] + .N()$betweenness_centrality[to]) %>%
      arrange(desc(edge_betweenness_sum)) %>%
      slice(1:50)
    
    top_50_graph <- tbl_graph(nodes = filtered_graph %>% activate(nodes), edges = top_50_links, directed = FALSE)
    
    top_nodes <- top_50_graph %>%
      activate(nodes) %>%
      as_tibble() %>%
      arrange(desc(betweenness_centrality)) %>%
      slice(1:10)
    
    graph_layout <- create_layout(top_50_graph, layout = "fr")
    
    list(graph_layout = graph_layout, top_nodes = top_nodes, top_50_links = top_50_links)
  })
  
  # Update the choices for selectInput dynamically
  observe({
    updateSelectInput(session, "selectedEntity", choices = c("All", processed_data()$top_nodes$id))
  })
  
  # Update the selection information text
  output$selectionInfo <- renderText({
    selected_entity <- input$selectedEntity
    selected_type <- input$selectedType
    if (selected_entity == "All") {
      "Currently showing all entities and their relationships. Larger nodes indicate entities with higher centrality, meaning they are more important for connecting different parts of the network."
    } else {
      paste("Currently showing", selected_entity, "and all", selected_type, "relationships. Larger nodes indicate entities with higher centrality, meaning they are more important for connecting different parts of the network.")
    }
  })
  
  output$kkNetworkPlot <- renderPlotly({
    req(processed_data(), input$selectedEntity, input$selectedType)
    
    graph_layout <- processed_data()$graph_layout
    selected_entity <- input$selectedEntity
    selected_type <- input$selectedType
    
    if (selected_entity == "All") {
      p <- ggraph(graph_layout) + 
        geom_edge_link(aes(width = edge_betweenness_sum / max(edge_betweenness_sum), 
                           alpha = edge_betweenness_sum / max(edge_betweenness_sum), 
                           text = paste("Edge between:", from, "and", to, "<br>Betweenness Sum:", edge_betweenness_sum)), 
                       color = "black") +
        geom_node_point(aes(size = betweenness_centrality, 
                            text = paste("ID:", id, "<br>Betweenness:", betweenness_centrality, "<br>Closeness:", closeness_centrality)), 
                        color = "lightblue", show.legend = FALSE) +
        geom_node_text(aes(x = x, y = y, label = id), repel = TRUE, size = 3, check_overlap = TRUE) +
        theme_void() +
        labs(title = "Top 50 Links in Atypical Business Transactions",
             subtitle = "Nodes sized by betweenness centrality")
    } else {
      filtered_edges <- processed_data()$top_50_links
      if (selected_type != "All") {
        filtered_edges <- filtered_edges %>%
          filter(type == selected_type)
      }
      
      filtered_graph_layout <- create_layout(tbl_graph(nodes = graph_layout %>% as_tibble(), edges = filtered_edges, directed = FALSE), layout = "fr")
      
      p <- ggraph(filtered_graph_layout) + 
        geom_edge_link(aes(width = edge_betweenness_sum / max(edge_betweenness_sum), 
                           alpha = edge_betweenness_sum / max(edge_betweenness_sum), 
                           text = paste("Edge between:", from, "and", to, "<br>Betweenness Sum:", edge_betweenness_sum)), 
                       color = "black") +
        geom_node_point(aes(size = betweenness_centrality, 
                            text = paste("ID:", id, "<br>Betweenness:", betweenness_centrality, "<br>Closeness:", closeness_centrality)), 
                        color = "lightblue", show.legend = FALSE) +
        geom_node_point(data = filtered_graph_layout %>% filter(id == selected_entity), color = "hotpink", size = 5) +
        geom_node_text(aes(x = x, y = y, label = id), repel = TRUE, size = 3, check_overlap = TRUE) +
        theme_void() +
        labs(title = "Top 50 Links in Atypical Business Transactions",
             subtitle = paste("Nodes sized by betweenness centrality for", selected_entity))
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(title = list(text = "Node Centrality"), orientation = "h", x = 0.5, xanchor = "center"))
  })
}

                                         