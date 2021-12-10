library(shiny)
library(shinydashboard)
library(DT)
library(knitr)
library(shinycssloaders)
library(shinythemes)
library (shinyWidgets)
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
             menuItem("Density",
                         tabName = "density"
             ),
            menuItem("Line",
                      tabName = "line"
                      ),
            menuItem("Multi Line",
                     tabName = 'multiline')
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
              h1(strong("Correlation"),
                 align = "center"),
              box(plotOutput("CorrMap"),
                  width = 15)
            )
    ),
    tabItem(tabName = "density",
            fluidPage(
              h1(strong("Income vs Enrollment"),
                 align = "center"),
              box(plotOutput("relation"), width = 15),
              box(
                selectInput("education","Enrollment:",
                            c("No High School",
                              "High School",
                              "Associate",
                              "Bachelors"),
                            width = 500,
                )
              ),
            )
    ),
    tabItem(tabName = "line",
            fluidPage(
              h1(strong("Trends"),
                 align = "center"),
              box(plotOutput("trend"), width = 15),
              box(
                selectInput("xfact","X Factor:",
                            c("Income",
                              "Decade Growth Rate",
                              "Poverty"),
                            width = 500,
                )
              ),
              box(
                selectInput("yfact","Y Factor:",
                            c("No High School",
                              "High School",
                              "Associate",
                              "Bachelors"),
                            width = 500,
                )
              ),
              box(
                selectInput("geo","State or County:",
                            c("State",
                              "County"),
                            width = 500,
                            )
              )
            )
    ),
    tabItem(
      tabName = "multiline",
      fluidPage(
        h2(strong("Qualification at Rural and Urban level")),
        align = "center"),
      box(plotOutput("mline"),
          width = 15),
      h6("Note: 0 - most rural  9 - most urban"),
      materialSwitch(inputId = "id", label = "Smooth?", status = "success")
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
                      c("No High School",
                        "High School",
                        "Associate",
                        "Bachelors"),
                      width = 500,
          )
        ),
        box(
          selectInput("geography","State or County:",
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
            textOutput("text"),
            tags$br(),
            h4("References",
               align = "left"),
            h6(em(tags$a(href = "Insidehighered.com","Insidehighered.com")),
               align = "left"),
            h6(em(tags$a(href = "https://www.ers.usda.gov/data-products/county-level-data-sets/","www.ers.usda.gov")),
               align = "left")
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
          tags$br()
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
  output$Incometable <- renderDataTable(income_tab)
  output$Edutable <- renderDataTable(edu_table)
  output$modelmarkdown <- renderUI({
    HTML(markdown::markdownToHTML(knit("Model.Rmd", quiet = TRUE)))
  })  
  output$CorrMap <- renderPlot({
      plot(inc_edu_county[3:7])
  })
  
  output$mline <- renderPlot({
    if (input$id == FALSE)
      ggplot(final_group) +
        geom_line(aes(x = Rural_urban_code,
                      y = Percentage,
                      color = Qualification)) +
        labs( x = "Rural Urban Code") 
    else
      ggplot(final_group, aes(Rural_urban_code, Percentage, colour=Qualification, group = Qualification))+
      geom_smooth(se = FALSE) +
      labs (x = "Rural Urban Code")
  })
  output$relation <- renderPlot({
    if (input$education == "No High_School")
      yvalue <- "Less_than_High_School"
    else if (input$education == "High School")
      yvalue <- "High_School_Only"
    else if (input$education == "Associate")
      yvalue <- "College_or_Associate"
    else
      yvalue <- "Bachelors"
    
    ggplot(Ct_Pop,
          aes(x = Ct_Pop$Income, y = Ct_Pop[[yvalue]])) +
      labs(x = "Income",
           y = input$education) +
      geom_hex(bins = 70) +
      scale_fill_continuous(type = "viridis") +
      theme_bw()
      
  })
  
  output$sstate <- renderPlot({
    
    if (input$columns == "No High_School")
      xcolumn <- "Less_than_High_School"
    else if (input$columns == "High School")
      xcolumn <- "High_School_Only"
    else if (input$columns == "Associate")
      xcolumn <- "College_or_Associate"
    else
      xcolumn <- "Bachelors"
    
    if (input$geography == "State")
      plot_usmap(data = St_Pop,
                values = xcolumn,
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
                 values = xcolumn,
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
    
    if (input$xfact == "Income")
      xfactor <- "Median_Household_Income_2019"
    else if (input$xfact == "Decade Growth Rate")
      xfactor <- "avg_decade_growth_rate"
    else
      xfactor <- "Percent_Poverty"
    
    if (input$yfact == "No High School")
      yfactor <- "Percent_No_Diploma"
    else if (input$yfact == "High School")
      yfactor <- "Percent_Diploma"
    else if (input$yfact == "Associate")
      yfactor <- "Percent_Associates"
    else (input$yfact == "Bachelors")
      yfactor <- "Percent_Bachelors"
      
    if (input$geo == "State")
      ggplot(full_state_table,
             aes(full_state_table[[xfactor]],full_state_table[[yfactor]])) +
             labs(x = input$xfact, y = input$yfact) +
        geom_smooth() +
        geom_line()
    else
      ggplot(full_county_table,
             aes(full_county_table[[xfactor]],full_county_table[[yfactor]])) +
              labs(x = input$xfact, y = input$yfact) +
      geom_smooth() +
      geom_line()
    
    
  })
  output$text <- renderText({
    paste("Our project is about investigating the effects of different
          social and economic factors on graduation and enrollment rates across
          the country. This will allow us to better understand why there is such
          a large gap in educational attainment throughout the US. To do this,
          we imported multiple relevant datasets, cleaned them up in RStudio and
          used a combination of visualizations and models to discover new insights.
          We then created several maps with the plot_usmap function and made
          a Shiny App where we displayed all of our findings. Our thoughts and 
          processes have been summarized in the Shiny Dashboard.")
  })
  output$atext <- renderText({
    paste("A fun loving super active personality who prefers outdoor than indoor.
          Loves all kind of sporting activity and enjoys self time.")
  })
  output$btext <- renderText({
    paste("Ehh... he's alright, I guess.")
  })
  
}


shinyApp(ui, server)
