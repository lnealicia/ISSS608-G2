# Server logic for VIP Network
ytgraphServer <- function(input, output, session, nodes, links) {
  # Process the data for VIP Network
  processed_data <- reactive({
    nodes_people <- nodes() %>% filter(entity2 == "Person")
    nodes_company <- nodes() %>% filter(entity2 == "Organization")
    links_owns <- links() %>% filter(event2 == "Owns")
    
    # Calculate the number of ownerships for each person
    nodes_people <- nodes_people %>%
      rowwise() %>%
      mutate(no_owns = sum(links_owns$source == id)) %>%
      ungroup()
    
    nodes_people$no_owns <- as.numeric(nodes_people$no_owns)
    
    # Calculate the unique counts of 'no_owns' and their corresponding counts and percentages
    owns_summary <- nodes_people %>%
      group_by(no_owns) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100)
    
    # Define the threshold for 'influential'
    vip_threshold <- 91
    
    # Filter to keep only influential people and select relevant columns
    vip <- nodes_people %>%
      filter(no_owns >= vip_threshold) %>%
      select(id, country, dob, last_edited_date, date_added, no_owns)
    
    # Filter links_owns to keep only those connections where the source is in the vip list
    vip_connections <- links_owns %>%
      filter(source %in% vip$id | target %in% vip$id) %>%
      select(source, target, start_date, end_date, last_edited_date, date_added)
    
    list(vip = vip, vip_connections = vip_connections)
  })
  
  # Update the choices for selectInput dynamically
  observe({
    updateSelectizeInput(session, "selectedVIP", choices = processed_data()$vip$id)
  })
  
  output$networkPlot <- renderVisNetwork({
    req(input$selectedVIP)  # Ensure a VIP is selected
    
    selectedVIP <- as.character(input$selectedVIP)
    vip_connections <- processed_data()$vip_connections
    all_vip_nodes <- processed_data()$vip
    
    # Ensure all nodes in selected_links are present in nodes
    all_nodes <- unique(c(vip_connections$source, vip_connections$target))
    missing_nodes <- setdiff(all_nodes, nodes()$id)
    
    nodes_combined <- nodes() %>%
      left_join(all_vip_nodes %>% select(id, no_owns), by = "id")
    
    if (length(missing_nodes) > 0) {
      missing_nodes_df <- data.frame(
        id = missing_nodes,
        entity2 = rep("Unknown", length(missing_nodes)),
        type = rep("Unknown", length(missing_nodes)),
        revenue = rep(NA, length(missing_nodes)),
        no_owns = rep(0, length(missing_nodes)),
        stringsAsFactors = FALSE
      )
      nodes_combined <- bind_rows(nodes_combined, missing_nodes_df)
    }
    
    # Filter nodes to include only the VIPs and their connections
    selected_nodes <- nodes_combined %>% filter(id %in% all_nodes)
    
    # Convert to visNetwork format
    vis_nodes <- selected_nodes %>%
      rename(label = id, group = entity2, title = type) %>%
      mutate(value = ifelse(is.na(no_owns), 10, no_owns),
             color = ifelse(group == "Person", "green", "blue"),
             title = paste0("ID: ", label, "<br>Size: ", value),
             color = ifelse(label == selectedVIP, "red", color))
    
    vis_edges <- vip_connections %>%
      rename(from = source, to = target) %>%
      mutate(arrows = "to")
    
    # Create visNetwork
    visNetwork(vis_nodes, vis_edges) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>%
      visPhysics(stabilization = TRUE) %>%
      visNodes(size = 20) %>%
      visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 1)))
  })
}
