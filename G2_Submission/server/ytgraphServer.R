ytGraphServer <- function(input, output, session, data) {
  output$ytGraphPlot <- renderPlot({
    # Extract the data
    nodes <- data()$nodes
    vip_connections <- data()$vip_connections
    
    # Create graph from VIP connections
    g_vip <- graph_from_data_frame(d = vip_connections, directed = TRUE)
    
    # Identify VIPs and Companies
    V(g_vip)$type <- ifelse(V(g_vip)$name %in% nodes$id, "VIP", "Company")
    
    # Define colors and sizes
    V(g_vip)$color <- ifelse(V(g_vip)$type == "VIP", "blue", "orange")
    V(g_vip)$size <- ifelse(V(g_vip)$type == "VIP", 8, 5)
    
    # Plot the network
    plot(g_vip, vertex.label = NA, vertex.size = V(g_vip)$size, edge.arrow.size = 0.5, 
         vertex.color = V(g_vip)$color, main = "VIP Connections Network")
  })
}
