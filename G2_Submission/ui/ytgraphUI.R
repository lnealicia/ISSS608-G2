ytGraphUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("ytGraphPlot"))
  )
}
