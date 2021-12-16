#Importing libraries

library(tidyverse)
library(readr)
library(readxl)
library(reactable)
library(scales)
library(hrbrthemes)
library(ggridges)
library(gganimate)
library(viridis)
library(gifski)
library(hexbin)
library (usmap)
library(shiny)
library(shinydashboard)
library(DT)
library(knitr)
library(shinycssloaders)
library(shinythemes)
library (shinyWidgets)

#Importing dataset

Unemployment_dataset <- read_excel("dataset/Unemployment.xlsx", range = "A5:CN3201")
Education_dataset <- read_excel("dataset/Education.xlsx", range = "A5:AU3209")
Population_dataset <- read_excel("dataset/PopulationEstimates.xlsx", range = "A2:H3203")
Poverty_dataset <- read_excel("dataset/PovertyEstimates.xls", range = "A5:AB3198")

#Tidying the tables

income_table <- Unemployment_dataset %>%
  select("FIPS_Code","Area_name","Median_Household_Income_2019")

income_tab <- income_table %>% 
  rename("Median Income" = Median_Household_Income_2019)

edu_table <- Education_dataset %>%
  select(c(1),c(3),c(44:47))%>%
  rename("FIPS_Code" = c(1), "Area_name" = c(2), "Percent_No_Diploma" = c(3), "Percent_Diploma" = c(4), "Percent_Associates" = c(5),"Percent_Bachelors" = c(6))

popn_table <- Population_dataset%>%
  select(-c(2),-c(6:7))%>%
  rename("FIPS_Code" = c(1),"Area_name" = c(2),"Rural_urban_code" = c(3),"Start_popn" = c(4), "End_popn" = c(5))%>%
  mutate(avg_decade_growth_rate = ((1+((End_popn-Start_popn)/Start_popn))^(1/3)-1))

pov_table <- Poverty_dataset%>%
  select(c(1),c(3),c(11))%>%
  rename("FIPS_Code" = c(1),"Percent_Poverty" = c(3))

#Merged Tables

inc_edu_state <- merge(x = income_table, y = edu_table, by = "FIPS_Code")%>%
  select(-c(4))%>%
  filter(endsWith(FIPS_Code,"000"))

inc_edu_county <- merge(x = income_table, y = edu_table, by = "FIPS_Code")%>%
  select(-c(4))%>%
  filter(!endsWith(FIPS_Code,"000"))

#Full Tables:

full_county_table <- Reduce(function(...) merge(..., all = TRUE, by = "FIPS_Code"),
                            list(income_table, edu_table, popn_table, pov_table))%>%
  select(-c(4),-c(9),-c(14))%>%
  filter(!endsWith(FIPS_Code,"000"))%>%
  drop_na()

full_state_table <- Reduce(function(...) merge(..., all = TRUE, by = "FIPS_Code"),
                           list(income_table, edu_table, popn_table, pov_table))%>%
  select(-c(4),-c(9),-c(14))%>%
  filter(endsWith(FIPS_Code,"000"))%>%
  select(-c(8))

fuller_tbl <- full_county_table %>% 
  extract(Area_name.x, c("County", "State"), "([^,]+), ([^)]+)")

multi_smooth <- function(x,y,x_name,y_name){
  ggplot(full_county_table, aes(x, y, colour = Rural_urban_code, group = Rural_urban_code))+
    geom_smooth(se = FALSE)+
    labs(y=y_name,x=x_name)
}

multi_point <- function(x,y,x_name,y_name){
  ggplot(full_county_table, aes(x, y, colour = Rural_urban_code))+
    geom_point()+
    labs(y=y_name,x=x_name)
}

#County Table Grouped By Urban Classification

bach_urban_class <- full_county_table %>%
  group_by(Rural_urban_code)%>%
  summarise_at(vars(Percent_Bachelors),funs(mean(Percent_Bachelors)))

no_diploma_urban_class <- full_county_table %>%
  group_by(Rural_urban_code)%>%
  summarise_at(vars(Percent_No_Diploma),funs(mean(Percent_No_Diploma)))

diploma_urban_class <- full_county_table %>%
  group_by(Rural_urban_code)%>%
  summarise_at(vars(Percent_Diploma),funs(mean(Percent_Diploma)))

assoc_urban_class <- full_county_table %>%
  group_by(Rural_urban_code)%>%
  summarise_at(vars(Percent_Associates),funs(mean(Percent_Associates)))

group_by_urb_table <- Reduce(function(...) merge(..., all = TRUE, by = "Rural_urban_code"),
                             list(bach_urban_class, no_diploma_urban_class, diploma_urban_class, assoc_urban_class))

final_group <- group_by_urb_table %>% 
  gather(Qualification, Percentage, Percent_Bachelors:Percent_Associates)

#Statewise Cleaning

St_Pop <- inc_edu_state[order(inc_edu_state$Area_name),]
St_Pop <- St_Pop[!(St_Pop$Area_name == "United States"),]
St_Pop$fips <- statepop$fips 
St_Pop$abbr <- statepop$abbr

St_Pop <- St_Pop %>% 
  rename(Income = Median_Household_Income_2019) %>% 
  rename(Less_than_High_School = `Percent_No_Diploma`) %>% 
  rename ( High_School_Only = `Percent_Diploma`) %>% 
  rename(College_or_Associate = `Percent_Associates`) %>% 
  rename(Bachelors = `Percent_Bachelors`)

#Countywise Cleaning

Ct_Pop <- inc_edu_county[order(inc_edu_county$Area_name),]
Ct_Pop <- Ct_Pop[order(Ct_Pop$FIPS_Code),]
Ct_Pop <- na.omit(Ct_Pop)
temp1 <- countypop
temp2 <- temp1[!(temp1$fips == "15005"),]
Ct_Pop$fips <- temp2$fips
Ct_Pop$abbr <- temp2$abbr
Ct_Pop <- Ct_Pop %>% 
  rename(Income = Median_Household_Income_2019) %>% 
  rename(Less_than_High_School = `Percent_No_Diploma`) %>% 
  rename ( High_School_Only = `Percent_Diploma`) %>% 
  rename(College_or_Associate = `Percent_Associates`) %>% 
  rename(Bachelors = `Percent_Bachelors`)

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
             menuItem("Density",
                         tabName = "density"
             ),
             menuItem("Line",
                      tabName = "line"
             ),
            menuItem("Scatter",
                      tabName = "scatter"
                      ),
            menuItem("Multi Line",
                     tabName = 'multiline')
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
    tabItem(tabName = "density",
            fluidPage(
              h1(strong("Income vs Enrollment"),
                 align = "center"),
              fluidRow(box(title = "Density",
                status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                plotOutput("relation")
                ),
              box(status = "primary",
                  solidHeader = TRUE,
                selectInput("education","Enrollment:",
                            c("No High School",
                              "High School",
                              "Associate",
                              "Bachelors"),
                            width = 500,
                )
              )),
            )
    ),
    tabItem(tabName = "scatter",
            fluidPage(
              h1(strong("Scatter Plot"),
                 align = "center"),
              fluidRow(
                box(title = "Scatter",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("scat")),
              box(status = "primary",
                  solidHeader = TRUE,
                selectInput("in1","Input 1:",
                            c("Income",
                              "Growth Rate",
                              "Poverty"
                              ),
                            width = 500,
                )
              ),
              box(status = "primary",
                  solidHeader = TRUE,
                selectInput("in2","Input 2:",
                            c("No High School",
                              "High School",
                              "Associate",
                              "Bachelors"),
                            width = 500,
                )
              )),
              
            )
    ),
    tabItem(tabName = "line",
            fluidPage(
              h1(strong("Trends"),
                 align = "center"),
              fluidRow(box(title = "Line",
                status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("trend")),
              box(status = "primary",
                  solidHeader = TRUE,
                selectInput("xfact","X Factor:",
                            c("Income",
                              "Decade Growth Rate",
                              "Poverty"),
                            width = 500,
                )
              ),
              box(status = "primary",
                  solidHeader = TRUE,
                selectInput("yfact","Y Factor:",
                            c("No High School",
                              "High School",
                              "Associate",
                              "Bachelors"),
                            width = 500,
                )
              ),
              box(status = "primary",
                  solidHeader = TRUE,
                selectInput("geo","State or County:",
                            c("State",
                              "County"),
                            width = 500,
                            )
              ))
            )
    ),
    tabItem(
      tabName = "multiline",
      fluidPage(
        h2(strong("Qualification at Rural and Urban level")),
        align = "center"),
      fluidRow(box(title = "Multi Line",
        status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
        plotOutput("mline")),
      h6("Note: 1 - most urban  9 - most rural"),
      box(status = "primary",
          solidHeader = TRUE,
        materialSwitch(inputId = "id", label = "Smooth?", status = "success")
      ))),
    tabItem(
      tabName = "map",
      fluidPage(
        h1(strong("Educational distribution"),
           align = "center"),
        fluidRow(box(title = "US Distribution Map",
          status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
          plotOutput("sstate")
          ),
        box(status = "primary",
            solidHeader = TRUE,
          selectInput("columns","Qualification:",
                      c("No High School",
                        "High School",
                        "Associate",
                        "Bachelors"),
                      width = 500,
          )
        ),
        box(status = "primary",
            solidHeader = TRUE,
          selectInput("geography","State or County:",
                      c("State",
                        "County"),
                      width = 500,
                      )
        ))
      )
    ),
  tabItem(tabName = "summary",
          fluidPage(
            h1(strong("Project Summary"),
               align = "center"),
            fluidRow(img(src = "Summ.jpg",
                height = 250,
                width = 500,
                style="display: block; margin-left: auto; margin-right: auto;")
                ),
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
tabItem(tabName = "about",
        fluidPage(
          h1(strong("About the authors"),
             align = "center"),
          tags$br(),
          h3("Mageshwaran Anbazhagan",
             align = "left"),
          textOutput("atext"),
          tags$br(),
          h3("Tyler Toren",
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
    if (input$education == "No High School")
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
    
    if (input$columns == "No High School")
      xcolumn <- "Less_than_High_School"
    else if (input$columns == "High School")
      xcolumn <- "High_School_Only"
    else if (input$columns == "Associate")
      xcolumn <- "College_or_Associate"
    else  if (input$columns == "Bachelors")
      xcolumn <- "Bachelors"
    
    if (input$geography == "State")
      plot_usmap(data = St_Pop,
                values = xcolumn,
                labels = TRUE) +
        scale_fill_gradient(low = "red",
                            high = "green",
                            name = "Percentage%",
                            label = scales::comma,
                            limits = c(0,65)) +
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
    else if (input$xfact == "Poverty")
      xfactor <- "Percent_Poverty"
    
    if (input$yfact == "No High School")
      yfactor <- "Percent_No_Diploma"
    else if (input$yfact == "High School")
      yfactor <- "Percent_Diploma"
    else if (input$yfact == "Associate")
      yfactor <- "Percent_Associates"
    else if (input$yfact == "Bachelors")
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
  output$scat <- renderPlot({
    
    if (input$in1 == "Income")
      xfactr <- "Median_Household_Income_2019"
    else if (input$in1 == "Growth Rate")
      xfactr <- "avg_decade_growth_rate"
    else if (input$in1 == "Poverty")
      xfactr <- "Percent_Poverty"
    
    if (input$in2 == "No High School")
      yfactr <- "Percent_No_Diploma"
    else if (input$in2 == "High School")
      yfactr <- "Percent_Diploma"
    else if (input$in2 == "Associate")
      yfactr <- "Percent_Associates"
    else if (input$in2 == "Bachelors")
      yfactr <- "Percent_Bachelors"
    
    ggplot(full_county_table, aes(
      full_county_table[[xfactr]],
      full_county_table[[yfactr]],
      colour = Rural_urban_code))+
      geom_point()+
      labs(x=input$in1, y=input$in2)
    

    
    
  })
  output$text <- renderText({
    paste("Our project is about investigating the effects of different
          social and economic factors on graduation and enrollment rates across
          the country. This will allow us to better understand why there is such
          a large gap in educational attainment. To do this,
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
    paste("A data science student who is also looking to someday become a 
    successful business owner. His hobbies include gaming, going out with friends
    and playing the guitar.")
  })
  
}


shinyApp(ui, server)
