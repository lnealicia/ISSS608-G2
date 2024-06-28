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

ytgraphServer <- source("server/ytgraphServer.R", local = TRUE)$value
aligraphServer <- source("server/aligraphServer.R", local = TRUE)$value

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
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("VIP Network", visNetworkOutput("networkPlot")),
                  tabPanel("Beneficiaries of SouthSeafood Express Corp", 
                           textOutput("summaryText"),
                           visNetworkOutput("competingNetwork"))
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
}

# Run the application 
shinyApp(ui = ui, server = server)
