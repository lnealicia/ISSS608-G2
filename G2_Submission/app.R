
pacman::p_load(tidytext, readtext, shinyjs, tidyverse, jsonlite, igraph, tidygraph, ggraph, visNetwork, clock, graphlayouts,plotly,ggiraph, shinyWidgets, ggtext)

source("helpers/Settings.R", local = TRUE)$value


# Data loading
data <- reactive({
  nodes <- fromJSON("data/mc3.json")
  links <- fromJSON("data/mc3.json")
})

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
      plotOutput("vipPlot"),
      plotOutput("vipPlot2025"),
      plotOutput("vipPlot2035"),
      tableOutput("dataTableNodes"),
      tableOutput("dataTableLinks")
    )
  )
)

ui <- tagList(
  useShinyjs(),
  stylesUI,
  navbarPage(
    title = "The Big Fish",
    fluid = TRUE,
    inverse = TRUE,
    collapsible = TRUE,
    corpGraphUI(supernetwork, "corpGraph"),
    networkGraphUI(supernetwork, "networkGraph"),
    influenceGraphUI(supernetwork, "influenceGraph"),
  )
)

server <- function(input, output, session) {
  moduleServer("corpGraph", corpGraphServer, session = session)
  moduleServer("networkGraph", networkGraphServer, session = session)
  moduleServer("influenceGraph", influenceGraphServer, session = session)
}

shinyApp(ui = ui, server = server)