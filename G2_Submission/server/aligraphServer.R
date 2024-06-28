# Server logic for Beneficiaries of SouthSeafood Express Corp
aligraphServer <- function(input, output, session, nodes, links) {
  # Process the data for competing businesses
  processed_competing_data <- reactive({
    req(input$year)
    year <- input$year
    previous_year <- year - 1
    
    competing_businesses <- nodes() %>%
      filter(entity3 == "FishingCompany")
    
    # Convert date columns to Date objects
    links_df <- links() %>%
      mutate(
        start_date = as.Date(start_date, format = "%Y-%m-%d"),
        end_date = as.Date(end_date, format = "%Y-%m-%d")
      )
    
    # Filter the links for the current year
    current_year_edges <- links_df %>%
      filter((source %in% competing_businesses$id | target %in% competing_businesses$id) & 
               year(start_date) <= year & (is.na(end_date) | year(end_date) >= year))
    
    # Count the number of links for each competing business in the current year
    current_link_counts <- current_year_edges %>%
      group_by(source) %>%
      summarise(current_link_count = n(), .groups = 'drop') %>%
      full_join(current_year_edges %>%
                  group_by(target) %>%
                  summarise(current_link_count = n(), .groups = 'drop'), 
                by = c("source" = "target"), suffix = c("_source", "_target")) %>%
      replace_na(list(current_link_count_source = 0, current_link_count_target = 0)) %>%
      mutate(total_current_links = current_link_count_source + current_link_count_target)
    
    # Filter the links for the previous year
    previous_year_edges <- links_df %>%
      filter((source %in% competing_businesses$id | target %in% competing_businesses$id) & 
               year(start_date) <= previous_year & (is.na(end_date) | year(end_date) >= previous_year))
    
    # Count the number of links for each competing business in the previous year
    previous_link_counts <- previous_year_edges %>%
      group_by(source) %>%
      summarise(previous_link_count = n(), .groups = 'drop') %>%
      full_join(previous_year_edges %>%
                  group_by(target) %>%
                  summarise(previous_link_count = n(), .groups = 'drop'), 
                by = c("source" = "target"), suffix = c("_source", "_target")) %>%
      replace_na(list(previous_link_count_source = 0, previous_link_count_target = 0)) %>%
      mutate(total_previous_links = previous_link_count_source + previous_link_count_target)
    
    # Merge the current and previous link counts with the competing businesses data
    competing_businesses <- competing_businesses %>%
      left_join(current_link_counts, by = c("id" = "source")) %>%
      left_join(previous_link_counts, by = c("id" = "source")) %>%
      mutate(total_current_links = replace_na(total_current_links, 0),
             total_previous_links = replace_na(total_previous_links, 0))
    
    # Determine activity levels based on the difference from the previous year
    competing_businesses <- competing_businesses %>%
      mutate(activity_status = ifelse(total_current_links > total_previous_links, "more_active", "neutral"))
    
    # Filter out nodes with no links
    competing_businesses <- competing_businesses %>%
      filter(total_current_links > 0)
    
    # Calculate the total number of nodes, active companies, and inactive companies
    total_nodes <- nrow(competing_businesses)
    active_companies <- nrow(competing_businesses %>% filter(activity_status == "more_active"))
    inactive_companies <- total_nodes - active_companies
    
    # Count the total links for "SouthSeafood Express Corp" in the current year
    southseafood_links <- competing_businesses %>%
      filter(id == "SouthSeafood Express Corp") %>%
      pull(total_current_links)
    
    list(competing_businesses = competing_businesses, current_year_edges = current_year_edges, 
         active_companies = active_companies, inactive_companies = inactive_companies, southseafood_links = southseafood_links)
  })
  
  output$competingNetwork <- renderVisNetwork({
    data <- processed_competing_data()
    competing_businesses <- data$competing_businesses
    current_year_edges <- data$current_year_edges
    
    vis_nodes <- competing_businesses %>%
      rename(label = id, group = entity2, title = type) %>%
      mutate(color = case_when(
        label == "SouthSeafood Express Corp" ~ "red",
        activity_status == "more_active" ~ "green",
        activity_status == "neutral" ~ "grey",
        TRUE ~ "blue"
      ),
      title = paste0("ID: ", label, "<br>Links: ", total_current_links))
    
    vis_edges <- current_year_edges %>%
      rename(from = source, to = target) %>%
      mutate(arrows = "to")
    
    visNetwork(vis_nodes, vis_edges) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1)) %>%
      visPhysics(stabilization = TRUE) %>%
      visNodes(size = 20) %>%
      visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 1)))
  })
  
  output$summaryText <- renderText({
    data <- processed_competing_data()
    active_companies <- data$active_companies
    inactive_companies <- data$inactive_companies
    southseafood_links <- data$southseafood_links
    
    paste("Total number of active companies:", active_companies,
          "Total number of inactive companies:", inactive_companies,
          "SouthSeafood Express Corp has", southseafood_links, "current links.")
  })
}
