
pacman::p_load(tidytext, readtext, quanteda, tidyverse, jsonlite, igraph, tidygraph, ggraph, visNetwork, clock, graphlayouts,plotly,ggiraph)

# Define UI
ui <- fluidPage(
  titlePanel("VIP Connections Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("filter_year", "Select Year:", choices = c(2022, 2025, 2035)),
      actionButton("update", "Update Plot")
    ),
    mainPanel(
      h2("VIP Connections Network Analysis"),
      textOutput("introText"),
      verbatimTextOutput("cleanedNodes"),
      verbatimTextOutput("cleanedLinks"),
      plotOutput("vipPlot2022"),
      plotOutput("vipPlot2025"),
      plotOutput("vipPlot2035"),
      tableOutput("dataTableNodes"),
      tableOutput("dataTableLinks")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Load and clean the data
  data <- reactive({
    nodes <- fromJSON("data/mc3.json", simplifyDataFrame = TRUE)
    links <- fromJSON("data/mc3.json", simplifyDataFrame = TRUE)
    
    # Replace NaN and NA with 0 for specific columns
    nodes<- nodes%>%
      mutate(across(everything(), ~ifelse(is.na(.) | . == "NaN" | . == "", 0, .)))
    
    links<- links%>%
      mutate(across(everything(), ~ifelse(is.na(.) | . == "NaN" | . == "", 0, .)))
    
    
    cleaned_nodes <- nodes %>%
      mutate(
        id = as.character(id),
        label = as.character(label),
        type = as.character(type),
        start_date = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S"),
        end_date = as.POSIXct(end_date, format = "%Y-%m-%dT%H:%M:%S")
      )
    
    cleaned_links <- links %>%
      mutate(
        source = as.character(source),
        target = as.character(target),
        type = as.character(type),
        start_date = as.POSIXct(start_date, format = "%Y-%m-%dT%H:%M:%S"),
        end_date = as.POSIXct(end_date, format = "%Y-%m-%dT%H:%M:%S")
      )
    
    cleaned_nodes <- cleaned_nodes %>%
      rename(
        "last_edited_by" = "_last_edited_by",
        "date_added" = "_date_added",
        "last_edited_date" = "_last_edited_date",
        "raw_source" = "_raw_source",
        "algorithm" = "_algorithm"
      )
    
    cleaned_links <- cleaned_links %>%
      rename(
        "last_edited_by" = "_last_edited_by",
        "date_added" = "_date_added",
        "last_edited_date" = "_last_edited_date",
        "raw_source" = "_raw_source",
        "algorithm" = "_algorithm"
      )
    
    list(cleaned_nodes = cleaned_nodes, cleaned_links = cleaned_links)
  })
  
  output$cleanedNodes <- renderPrint({
    glimpse(data()$cleaned_nodes)
  })
  
  output$cleanedLinks <- renderPrint({
    glimpse(data()$cleaned_links)
  })
  
  observeEvent(input$update, {
    filter_year <- input$filter_year
    
    # Filter nodes and links
    filtered_nodes <- data()$cleaned_nodes %>%
      filter(format(start_date, "%Y") == filter_year)
    
    filtered_links <- data()$cleaned_links %>%
      filter(format(start_date, "%Y") == filter_year)
    
    word_list1 <- strsplit(filtered_nodes$type, "\\.")
    max_elements1 <- max(lengths(word_list1))
    word_list_padded1 <- lapply(word_list1, function(x) c(x, rep(NA, max_elements1 - length(x))))
    word_df1 <- do.call(rbind, word_list_padded1)
    colnames(word_df1) <- paste0("entity", 1:max_elements1)
    word_df1 <- as_tibble(word_df1) %>% select(entity2, entity3)
    
    word_list <- strsplit(filtered_links$type, "\\.")
    max_elements <- max(lengths(word_list))
    word_list_padded <- lapply(word_list, function(x) c(x, rep(NA, max_elements - length(x))))
    word_df <- do.call(rbind, word_list_padded)
    colnames(word_df) <- paste0("entity", 1:max_elements)
    word_df <- as_tibble(word_df) %>% select(entity2, entity3)
    
    # Create graph for 2022
    if (filter_year == 2022) {
      vip_connections_filtered_2022 <- data()$cleaned_links %>%
        filter(format(start_date, "%Y") == filter_year)
      
      g_vip_filtered_2022 <- graph_from_data_frame(d = vip_connections_filtered_2022, directed = TRUE)
      
      V(g_vip_filtered_2022)$type <- ifelse(V(g_vip_filtered_2022)$name %in% data()$cleaned_nodes$id, "VIP", "Company")
      V(g_vip_filtered_2022)$color <- ifelse(V(g_vip_filtered_2022)$type == "VIP", "blue", "orange")
      V(g_vip_filtered_2022)$size <- ifelse(V(g_vip_filtered_2022)$type == "VIP", 8, 5)
      
      output$vipPlot2022 <- renderPlot({
        plot(g_vip_filtered_2022, vertex.label = NA, vertex.size = V(g_vip_filtered_2022)$size, edge.arrow.size = 0.5, 
             vertex.color = V(g_vip_filtered_2022)$color, main = paste("VIP Connections Network for", filter_year))
      })
    }
    
    # Create graph for 2025
    if (filter_year == 2025) {
      vip_connections_filtered_2025 <- data()$cleaned_links %>%
        filter(format(start_date, "%Y") == filter_year)
      
      g_vip_filtered_2025 <- graph_from_data_frame(d = vip_connections_filtered_2025, directed = TRUE)
      
      V(g_vip_filtered_2025)$type <- ifelse(V(g_vip_filtered_2025)$name %in% data()$cleaned_nodes$id, "VIP", "Company")
      V(g_vip_filtered_2025)$color <- ifelse(V(g_vip_filtered_2025)$type == "VIP", "blue", "orange")
      V(g_vip_filtered_2025)$size <- ifelse(V(g_vip_filtered_2025)$type == "VIP", 8, 5)
      
      output$vipPlot2025 <- renderPlot({
        plot(g_vip_filtered_2025, vertex.label = NA, vertex.size = V(g_vip_filtered_2025)$size, edge.arrow.size = 0.5, 
             vertex.color = V(g_vip_filtered_2025)$color, main = paste("VIP Connections Network for", filter_year))
      })
    }
    
    # Create graph for 2035
    if (filter_year == 2035) {
      vip_connections_filtered_2035 <- data()$cleaned_links %>%
        filter(format(start_date, "%Y") == filter_year)
      
      g_vip_filtered_2035 <- graph_from_data_frame(d = vip_connections_filtered_2035, directed = TRUE)
      
      V(g_vip_filtered_2035)$type <- ifelse(V(g_vip_filtered_2035)$name %in% data()$cleaned_nodes$id, "VIP", "Company")
      V(g_vip_filtered_2035)$color <- ifelse(V(g_vip_filtered_2035)$type == "VIP", "blue", "orange")
      V(g_vip_filtered_2035)$size <- ifelse(V(g_vip_filtered_2035)$type == "VIP", 8, 5)
      
      output$vipPlot2035 <- renderPlot({
        plot(g_vip_filtered_2035, vertex.label = NA, vertex.size = V(g_vip_filtered_2035)$size, edge.arrow.size = 0.5, 
             vertex.color = V(g_vip_filtered_2035)$color, main = paste("VIP Connections Network for", filter_year))
      })
    }
    
    output$dataTableNodes <- renderTable({
      data()$cleaned_nodes
    })
    
    output$dataTableLinks <- renderTable({
      data()$cleaned_links
    })
  })
  
  output$introText <- renderText({
    "This is an analysis of VIP connections network from 2022 to 2035."
  })
}

# Run the application
shinyApp(ui = ui, server = server)