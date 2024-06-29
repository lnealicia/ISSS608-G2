# Load required libraries
pacman::p_load(
  shiny, tidytext, readtext, shinyjs, tidyverse, jsonlite, 
  igraph, tidygraph, ggraph, visNetwork, clock, graphlayouts,
  plotly, ggiraph, shinyWidgets, ggtext, lubridate
)

# Source UI and server components
ytgraphUI <- source("ui/ytgraphUI.R", local = TRUE)$value
aligraphUI <- source("ui/aligraphUI.R", local = TRUE)$value
stylesUI <- source("ui/styles.R", local = TRUE)$value
network_ui <- source("ui/network_ui.R", local = TRUE)$value

ytgraphServer <- source("server/ytgraphServer.R", local = TRUE)$value
aligraphServer <- source("server/aligraphServer.R", local = TRUE)$value
network_server <- source("server/network_server.R", local = TRUE)$value

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
        aligraphUI
      ),
      conditionalPanel(
        condition = "input.tabs == 'Directed Graph'",
        network_ui("directedGraph")  # Use network_ui for Directed Graph tab
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("VIP Network", visNetworkOutput("networkPlot")),
                  tabPanel("Beneficiaries of SouthSeafood Express Corp", 
                           textOutput("summaryText"),
                           visNetworkOutput("competingNetwork")
                  ),
                  tabPanel("Directed Graph", 
                           plotOutput("directedGraph-graphPlot")  # Reference plot output for directed graph
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
  callModule(network_server, "directedGraph", nodes, links, selected_companies)
}

# Run the application 
shinyApp(ui = ui, server = server)
