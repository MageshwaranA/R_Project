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
             startExpanded = FALSE
),
    menuItem("Dataset",
             tabName = "dataset",
             icon = icon("database"),
             startExpanded = FALSE,
             menuSubItem("Income", tabName = "income"),
             menuSubItem("Education", tabName = "education")
             ),
    menuItem("Map",
             tabName = "map",
             icon = icon("map-pin")
    ),
    menuItem("Plots",
             tabName = "plots",
             icon = icon("chart-bar"),
             startExpanded = FALSE,
             menuItem("Correlation",
                      tabName = "Corrmap"
                      ),
             menuItem("Visual",
                         tabName = "visual",
                         startExpanded = FALSE,
                         menuSubItem(
                           "Density",
                           tabName = "density"
                         ),
                      menuSubItem(
                        "Graphs",
                        tabName = "graphs"
                      ))
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
              h1(strong("Income"),
                 align = "center"),
              dataTableOutput("Incometable")
            )
    ),
    tabItem(tabName = "education",
            fluidPage(
              h1(strong("Education"),
                 align = "center"),
              dataTableOutput("Edutable")
            )
    ),
    tabItem(tabName = "Corrmap",
            fluidPage(
              h1(strong("Correlation Graph"),
                 align = "center"),
              box(plotOutput("CorrMap"), width = 15)
            )
    ),
    tabItem(tabName = "density",
            fluidPage(
              h1(strong("Income vs Enrollment"),
                 align = "center"),
              box(plotOutput("relation"), width = 15),
              box(
                selectInput("education","Enrollment:",
                            c("Less_than_High_School",
                              "High_School_Only",
                              "College_or_Associate",
                              "Bachelors"),
                            width = 500,
                )
              ),
            )
    ),
    tabItem(tabName = "graphs",
            fluidPage(
              h1(strong("Trends"),
                 align = "center"),
              box(plotOutput("trend"), width = 15),
              box(
                selectInput("xfact","X Factor:",
                            c("Median_Household_Income_2019",
                              "avg_decade_growth_rate",
                              "Percent_Poverty"),
                            width = 500,
                )
              ),
              box(
                selectInput("yfact","Y Factor:",
                            c("Percent_No_Diploma",
                              "Percent_Diploma",
                              "Percent_Associates",
                              "Percent_Bachelors"),
                            width = 500,
                )
              ),
              box(
                selectInput("geo","Geography:",
                            c("State",
                              "County"),
                            width = 500,
                            )
              )
            )
    ),
    tabItem(
      tabName = "map",
      fluidPage(
        h1(strong("Educational distribution"),
           align = "center"),
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
        ),
        box(
          selectInput("geography","Geography:",
                      c("State",
                        "County"),
                      width = 500,
                      )
        )
      )
    ),
  tabItem(tabName = "summary",
          fluidPage(
            h1(strong("Project Summary"),
               align = "center"),
            img(src = "Summ.jpg",
                height = 250,
                width = 500,
                style="display: block; margin-left: auto; margin-right: auto;"),
            tags$br(),
            textOutput("text")
          )),
tabItem(tabName = "model",
        fluidPage(
          h1(strong("Linear Model"),
             align = "center"),
          uiOutput("modelmarkdown")
        )),
tabItem(tabName = "about",
        fluidPage(
          h1(strong("About the authors"),
             align = "center"),
          tags$br(),
          h3("Mageshwaran Anbazhagan",
             align = "left"),
          textOutput("atext"),
          tags$br(),
          h3("Tyler Torren",
             align = "left"),
          textOutput("btext"),
          tags$br(),
          tags$br(),
          h4("References",
             align = "left"),
          h6(em(tags$a(href = "Insidehighered.com","Insidehighered.com")),
             align = "left"),
          h6(em(tags$a(href = "https://www.ers.usda.gov/data-products/county-level-data-sets/","www.ers.usda.gov")),
             align = "left")
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
  output$modelmarkdown <- renderUI({
    HTML(markdown::markdownToHTML(knit("Model.Rmd", quiet = TRUE)))
  })  
  output$CorrMap <- renderPlot({
      plot(inc_edu_county[3:7])
  })
  output$relation <- renderPlot({
    yvar <- input$education
      ggplot(Ct_Pop,
           aes(x = Ct_Pop$Income, y = Ct_Pop[[yvar]])) +
      geom_hex(bins = 70) +
      scale_fill_continuous(type = "viridis") +
      theme_bw()
      
  })
  #Tyler

  
  output$sstate <- renderPlot({
    if (input$geography == "State")
      plot_usmap(data = St_Pop,
                values = input$columns,
                labels = TRUE) +
        scale_fill_gradient(low = "red",
                            high = "green",
                            name = "Percentage%",
                            label = scales::comma,
                            limits = c(0,100)) +
        ggtitle(input$columns) +
        theme(plot.title = element_text(size = 15,
                                        hjust = 0.5)) +
        theme(legend.position = "right",
              legend.text = element_text(size = 06))
    else
      plot_usmap(data = Ct_Pop,
                 values = input$columns,
                 labels = TRUE) +
      scale_fill_gradient(low = "red",
                          high = "green",
                          name = "Percentage%",
                          label = scales::comma,
                          limits = c(0,100)) +
      ggtitle(input$column) +
      theme(plot.title = element_text(size = 15,
                                      hjust = 0.5)) +
      theme(legend.position = "right",
            legend.text = element_text(size = 06))
      
  })
  output$trend <- renderPlot({
    
    if (input$geo == "State")
      ggplot(full_state_table,
             aes(full_state_table[[input$xfact]],full_state_table[[input$yfact]])) +
             labs(x = input$xfact, y = input$yfact) +
        geom_smooth()
    else
      ggplot(full_county_table,
             aes(full_county_table[[input$xfact]],full_county_table[[input$yfact]])) +
              labs(x = input$xfact, y = input$yfact) +
      geom_smooth()
    
    
  })
  output$text <- renderText({
    paste("We will be investigating the effects of Income on the student enrollment to different programs. We have imported relevant dataset to support our study, cleaned and organized the dataframe according to our preference to establish an relationship.
          We have grouped the findings as per state and counties in the United States of America and visualized it through the use of plot_usmap function.
          This plot shows the distribution percentage of student enrollment to different programs in a county or state.
          Furthermore we have created correlation plot to identify the connection between income and programs enrolled, it is also made dynamic to make it easier to understand the link between two variables.
          In addition to it, we have performed linear model to get a deeper knowledge on the relation.
          We have put together all our findings in one place with the help of 'Shiny'.
          We have used shiny dashboard to summarize our thoughts and process.")
  })
  output$atext <- renderText({
    paste("A fun loving super active personality who prefers outdoor than indoor.
          Loves all kind of sporting activity and enjoys self time.")
  })
  output$btext <- renderText({
    paste("Needs to be updated")
  })
  
}


shinyApp(ui, server)
