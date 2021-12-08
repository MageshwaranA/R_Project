library(shiny)
library(shinydashboard)
library(DT)
library(knitr)
library(shinycssloaders)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Summary",
             tabName = "summary",
             icon = icon("lightbulb"),
             startExpanded = FALSE,
             menuSubItem("Project",
                         tabName = "project"),
             menuSubItem("Code",
                         tabName = "code")),
    menuItem("Dataset",
             tabName = "dataset",
             icon = icon("database"),
             startExpanded = FALSE,
             menuSubItem("Income", tabName = "income"),
             menuSubItem("Education", tabName = "education")
             ),
    menuItem("Map",
             tabName = "map",
             icon = icon("map-pin"),
             startExpanded = FALSE,
             menuSubItem("State Level", tabName = "state"),
             menuSubItem("County Level", tabName = "county")
    ),
    menuItem("Graph",
             tabName = "graph",
             icon = icon("chart-bar"),
             startExpanded = FALSE,
             menuItem("Correlation", tabName = "Cmap",
                      startExpanded = FALSE,
                      menuSubItem("Dynamic Correlation",
                                  tabName = "dcor"),
                      menuSubItem("Fixed Correlation",
                                  tabName = "Corrmap")),
             menuSubItem("Visual", tabName = "visual")
    ),
    menuItem("Model",
             tabName = "model",
             icon = icon("clipboard")
    ),
    menuItem("About", tabName = "about", icon = icon("address-card"))
  )
)

body <- dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "income",
            fluidPage(
              h1("Income"),
              dataTableOutput("Incometable")
            )
    ),
    tabItem(tabName = "education",
            fluidPage(
              h1("Education"),
              dataTableOutput("Edutable")
            )
    ),
    tabItem(tabName = "Corrmap",
            fluidPage(
              h1("Correlation Graph"),
              box(plotOutput("CorrMap"), width = 15)
            )
    ),
    tabItem(tabName = "dcor",
            fluidPage(
              h1("Income vs Enrollment"),
              box(plotOutput("relation"), width = 15),
              box(
                selectInput("education","Enrollment:",
                            c("Percent_No_Diploma",
                              "Percent_Diploma",
                              "Percent_Associates",
                              "Percent_Bachelors"),
                            width = 500,
                )
              )
            )
    ),
    tabItem(
      tabName = "state",
      fluidPage(
        h1("Educational distribution"),
        box(plotOutput("sstate"),
            width = 15),
        box(
          selectInput("columns","Qualification:",
                      c("Less_than_High_School",
                        "High_School_Only",
                        "College_or_Associate",
                        "Bachelors"),
                      width = 500,
          )
        )
      )
    ),
    tabItem(
      tabName = "county",
      fluidPage(
        h1("Educational distribution"),
        box(plotOutput("ccounty"),
            width = 15),
        box(
          selectInput("column","Qualification:",
                      c("Less_than_High_School",
                        "High_School_Only",
                        "College_or_Associate",
                        "Bachelors"),
                      width = 500,
          )
        )
      )
    ),
    tabItem(tabName = "code",
            fluidPage(
              h2("Code Walkthrough"),
              uiOutput("markdown")
              
  )
),
  tabItem(tabName = "project",
          fluidPage(
            h2("Project Summary"),
            textOutput("text")
          )),
tabItem(tabName = "model",
        fluidPage(
          h2("Linear Model"),
          uiOutput("modelmarkdown")
        )),
tabItem(tabName = "about",
        fluidPage(
          h2("About the authors"),
          textOutput("atext")
        ))
)
)

ui <- dashboardPage(
  dashboardHeader(
    title = "Dashboard"),
  sidebar,
  body
)



server <- function(input, output) {
  output$Incometable <- renderDataTable(income_table)
  output$Edutable <- renderDataTable(edu_table)
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit("Public Vs Private Enrollment Countywide data.Rmd", quiet = TRUE)))
  })
  output$modelmarkdown <- renderUI({
    HTML(markdown::markdownToHTML(knit("Model.Rmd", quiet = TRUE)))
  })  
  output$CorrMap <- renderPlot({
      plot(inc_edu_county[3:7])
  })
  output$relation <- renderPlot({
    plot(inc_edu_county$Median_Household_Income_2019,inc_edu_county[[input$education]],
             xlab = "Income",
             ylab = input$education)
    
  })
  output$sstate <- renderPlot({
    plot_usmap(data = St_Pop,
               values = input$columns,
               labels = TRUE) +
      scale_fill_gradient(low = "red",
                          high = 'green',
                          name = "Percentage%",
                          label = scales::comma,
                          limits = c(0,100)) +
      ggtitle(input$columns) +
      theme(plot.title = element_text(size = 15,
                                      hjust = 0.5)) +
      theme(legend.position = "right",
            legend.text = element_text(size = 06))
  })
  output$ccounty <- renderPlot({
    plot_usmap(data = Ct_Pop,
               values = input$column,
               labels = TRUE) +
      scale_fill_gradient(low = "red",
                          high = 'green',
                          name = "Percentage%",
                          label = scales::comma,
                          limits = c(0,100)) +
      ggtitle(input$column) +
      theme(plot.title = element_text(size = 15,
                                      hjust = 0.5)) +
      theme(legend.position = "right",
            legend.text = element_text(size = 06))
  })
  output$text <- renderText({
    paste("We will be investigating the effects of Income on the student enrollment.
          a")
  })
  output$atext <- renderText({
    paste("About the authors")
  })
}


shinyApp(ui, server)
