library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(
    title = "Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "dataset", icon = icon("database")),
      menuItem("Map", tabName = "map", icon = icon("map-pin")),
      menuItem("Graph", tabName = "graph", icon = icon("chart-bar")),
      menuItem("Summary", tabName = "summary", icon = icon("lightbulb")),
      menuItem("About", tabName = "about", icon = icon("address-card"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dataset",
              fluidPage(
                h1("Income"),
                dataTableOutput("Incometable")
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  output$Incometable <- renderDataTable(income_table)
}

shinyApp(ui, server)