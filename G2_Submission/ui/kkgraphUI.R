# UI for Atypical Transactions
tagList(
  selectInput("selectedEntity", "Select Entity", choices = NULL),
  selectInput("selectedType", "Select Type", choices = c("All", "Event.Owns.Shareholdership", "Event.WorksFor", "Event.Owns.BeneficialOwnership")),
  textOutput("selectionInfo"),
  br(),
  p("Betweenness Centrality: Measures the extent to which a node lies on the shortest paths between other nodes. Higher betweenness centrality indicates that a node is more critical for the flow of information through the network."),
  p("Closeness Centrality: Measures how close a node is to all other nodes in the network. Higher closeness centrality indicates that a node can quickly interact with all other nodes.")
)
