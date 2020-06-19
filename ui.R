library(shiny)
library(tidyverse)

source("data_processing.R")

navbarPage(
  
  'COVID-19', 
  
  
  tabPanel("World",
           
           fluidPage(fluidRow(
                column(selectInput('continent',
                              label = 'Continent',
                              choices = NULL),
                       width = 4),
                column(selectInput('country',
                                label = 'Country',
                                choices = NULL),
                       width = 4),
                column(selectInput('indicator_country',
                                   label = 'Select Indicator',
                                   choices = c("New Cases/Deaths",
                                               "Cumulative Cases/Deaths (Linear)",
                                               "Cumulative Cases/Deaths (Logistic)")),
                       width = 4)
          )),
           plotOutput("cases_country"),
           plotOutput('deaths_country')
    ),
 
  tabPanel("States",
         
           
      fluidPage(fluidRow(
              column(selectInput("state",
                                label = "State", 
                                choices = unique(states$state)),
                      width = 6),
              column(selectInput('indicator_state',
                                 label = 'Select Indicator',
                                 choices = c("New Cases/Deaths",
                                              "Cumulative Cases/Deaths (Linear)",
                                              "Cumulative Cases/Deaths (Logistic)")),
                     width = 6)
            )),
        
            #Note: make these generic
            plotOutput("state_cases"),
            plotOutput("state_deaths")
        
    ),
  
  tabPanel("Counties",
      fluidPage(fluidRow(
            column(selectInput('state_filter',
                                label = 'State',
                                choices = NULL),
                  width = 4),
            column(selectInput('county',
                              label = 'County',
                              choices = NULL),
                   width = 4),
            column(selectInput('indicator_county',
                               label = 'Select Indicator',
                               choices = c("New Cases/Deaths",
                                           "Cumulative Cases/Deaths (Linear)",
                                           "Cumulative Cases/Deaths (Logistic)")),
                   width = 4)
      )), 
    plotOutput("county_cases"),
    plotOutput("county_deaths")
  ),

  collapsible = TRUE
  
)