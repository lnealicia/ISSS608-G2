graphUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("vipGraphPlot"))
  )
}
