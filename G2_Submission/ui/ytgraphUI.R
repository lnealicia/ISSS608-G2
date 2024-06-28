ytgraphUI <- fluidPage(
  selectInput("startingAlphabet", "Select Starting Alphabet", 
              choices = c(LETTERS, "0-9"), selected = "A"),
  selectizeInput("selectedVIP", "Select a VIP", choices = NULL)
)
