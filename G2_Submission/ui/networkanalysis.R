library(shiny)

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
