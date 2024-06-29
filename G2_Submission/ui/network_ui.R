# UI for Directed Graph
network_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("year"), "Select Year", min = 2005, max = 2035, value = 2020, step = 1),  # Year selection
    plotOutput(ns("graphPlot"))  # Plot output for the directed graph
  )
}
