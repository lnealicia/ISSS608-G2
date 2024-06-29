# # Load required libraries
# pacman::p_load(
#   shiny, tidytext, readtext, shinyjs, tidyverse, jsonlite, 
#   igraph, tidygraph, ggraph, visNetwork, clock, graphlayouts,
#   plotly, ggiraph, shinyWidgets, ggtext, lubridate
# )
# 
# # Source UI and server components
# ytgraphUI <- source("ui/ytgraphUI.R", local = TRUE)$value
# aligraphUI <- source("ui/aligraphUI.R", local = TRUE)$value
# stylesUI <- source("ui/styles.R", local = TRUE)$value
# 
# ytgraphServer <- source("server/ytgraphServer.R", local = TRUE)$value
# aligraphServer <- source("server/aligraphServer.R", local = TRUE)$value
# 
# # Define UI
# ui <- fluidPage(
#   titlePanel("Red Herring"),
#   stylesUI,
#   sidebarLayout(
#     sidebarPanel(
#       conditionalPanel(
#         condition = "input.tabs == 'VIP Network'",
#         ytgraphUI
#       ),
#       conditionalPanel(
#         condition = "input.tabs == 'Beneficiaries of SouthSeafood Express Corp'",
#         aligraphUI
#       )
#     ),
#     mainPanel(
#       tabsetPanel(id = "tabs",
#                   tabPanel("VIP Network", visNetworkOutput("networkPlot")),
#                   tabPanel("Beneficiaries of SouthSeafood Express Corp", 
#                            textOutput("summaryText"),
#                            visNetworkOutput("competingNetwork"))
#       )
#     )
#   )
# )
# 
# # Define server logic
# server <- function(input, output, session) {
#   # Data loading
#   nodes <- reactive({
#     readRDS("data/rds/cleaned_nodes.rds")
#   })
#   
#   links <- reactive({
#     readRDS("data/rds/cleaned_links.rds")
#   })
#   
#   # Call module servers
#   ytgraphServer(input, output, session, nodes, links)
#   aligraphServer(input, output, session, nodes, links)
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

##STRATS HERE
#Load required libraries
pacman::p_load(
  shiny, tidytext, readtext, shinyjs, tidyverse, jsonlite,
  igraph, tidygraph, ggraph, visNetwork, clock, graphlayouts,
  plotly, ggiraph, shinyWidgets, ggtext, lubridate
)

# Source UI and server components
ytgraphUI <- source("ui/ytgraphUI.R", local = TRUE)$value
aligraphUI <- source("ui/aligraphUI.R", local = TRUE)$value
networkui <- source("ui/network_ui.R", local= TRUE)$value
stylesUI <- source("ui/styles.R", local = TRUE)$value

ytgraphServer <- source("server/ytgraphServer.R", local = TRUE)$value
aligraphServer <- source("server/aligraphServer.R", local = TRUE)$value
networkServer <- source("server/network_server.R", local = TRUE)$value

# List of companies to include in the graph
selected_companies <- c("Cortez LLC", "Evans-Pearson", "Friedman, Gibson and Garcia", "GvardeyskAmerica Shipping Plc",
                        "Hill PLC", "Howell LLC", "Johnson, Perez and Salinas", "Kaiser, Warren and Shepard",
                        "King and Sons", "Lane Group", "Lee-Ramirez", "Mcpherson-Wright", "NamRiver Transit A/S",
                        "Osborne, Saunders and Brown", "Patel-Miller", "Ramos, Jordan and Stewart",
                        "Rivera, Lee and Carroll", "Russell and Sons", "Stein, Taylor and Williams",
                        "StichtingMarine Shipping Company", "Vasquez-Gonzalez")

# Define UI
ui <- fluidPage(
  titlePanel("Red Herring"),
  stylesUI,
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'VIP Network'",
        ytgraphUI
      ),
      conditionalPanel(
        condition = "input.tabs == 'Beneficiaries of SouthSeafood Express Corp'",
        aligraphUI,
        sliderInput("year", "Select Year", min = 2005, max = 2035, value = 2020, step = 1)  # Add year selection
      ),
      conditionalPanel(
        condition = "input.tabs == 'Hello'",
        networkui,
        sliderInput("year", "Select Year", min = 2005, max = 2035, value = 2020, step = 1)  # Add year selection
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("VIP Network", visNetworkOutput("networkPlot")),
                  tabPanel("Beneficiaries of SouthSeafood Express Corp",
                           textOutput("summaryText"),
                           visNetworkOutput("competingNetwork"),
                           plotOutput("graphPlot")  # Add plot output for the directed graph
                  ),
                  tabPanel("Beneficiaries of SouthSeafood Express Corp 2",
                           textOutput("summaryText"),
                           visNetworkOutput("competingNetwork"),
                           plotOutput("graphPlot")  # Add plot output for the directed graph
                  )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Data loading
  nodes <- reactive({
    readRDS("data/rds/cleaned_nodes.rds")
  })

  links <- reactive({
    readRDS("data/rds/cleaned_links.rds")
  })

  # Call module servers
  ytgraphServer(input, output, session, nodes, links)
  aligraphServer(input, output, session, nodes, links)
  networkServer(input, output, session, nodes, links)

  # Reactive expression to generate the graph data
  graph_data <- reactive({
    req(input$year)
    year <- input$year

    # Load nodes and links data
    nodes <- nodes() %>% filter(id %in% selected_companies)
    links <- links()

    # Filter edges based on selected year and selected companies
    edges_filtered <- links %>%
      filter((source %in% nodes$id & target %in% nodes$id) &
               year(start_date) <= year & (is.na(end_date) | year(end_date) >= year))

    # Create igraph object
    graph <- graph_from_data_frame(d = edges_filtered, vertices = nodes, directed = TRUE)
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

# Run the application
shinyApp(ui = ui, server = server)
