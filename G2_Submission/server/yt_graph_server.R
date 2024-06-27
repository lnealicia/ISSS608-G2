library(shiny)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(viridis)

# Load data (assuming mc3_nodes and mc3_edges are available in the environment)
# mc3_nodes <- read.csv("path_to_mc3_nodes.csv")
# mc3_edges <- read.csv("path_to_mc3_edges.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Business Network Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("linkType", "Select Link Type:", 
                  choices = c("Shareholdership", "WorksFor", "BeneficialOwnership", "FamilyRelationship")),
      checkboxGroupInput("nodeType", "Select Node Types:", 
                         choices = c("Company", "LogisticsCompany", "FishingCompany", "FinancialCompany", 
                                     "NewsCompany", "NGO", "Person", "CEO"),
                         selected = c("Company", "LogisticsCompany", "FishingCompany", "FinancialCompany", 
                                      "NewsCompany", "NGO", "Person", "CEO"))
    ),
    
    mainPanel(
      plotOutput("networkPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to filter data based on user input
  filtered_data <- reactive({
    link_type <- input$linkType
    node_types <- input$nodeType
    
    # Filter edges
    filtered_edges <- mc3_edges %>%
      filter(type %in% link_type)
    
    # Extract and filter nodes based on edges
    id1 <- filtered_edges %>%
      select(source) %>%
      rename(id = source)
    
    id2 <- filtered_edges %>%
      select(target) %>%
      rename(id = target)
    
    filtered_nodes <- rbind(id1, id2) %>%
      distinct() %>%
      left_join(mc3_nodes, by = c("id" = "id")) %>%
      filter(type %in% node_types)
    
    list(nodes = filtered_nodes, edges = filtered_edges)
  })
  
  output$networkPlot <- renderPlot({
    data <- filtered_data()
    
    # Create graph object
    mc3_graph <- tbl_graph(nodes = data$nodes, edges = data$edges, directed = TRUE) %>%
      mutate(betweenness_centrality = centrality_betweenness(),
             closeness_centrality = centrality_closeness())
    
    # Display the network graph
    ggraph(mc3_graph, layout = "fr") + # Using Fruchterman-Reingold layout
      geom_edge_link(aes(edge_alpha = 0.9, edge_width = 0.1)) + # Customize edge appearance
      geom_node_point(aes(size = betweenness_centrality, color = closeness_centrality)) + # Customize node appearance
      scale_color_viridis_c() + # Use viridis color scale
      theme_void() + # Use a void theme
      labs(title = "Network Graph of Business Network",
           subtitle = "Nodes colored by closeness centrality and sized by betweenness centrality",
           caption = "Data Source: mc3.json") # Add titles and captions
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
