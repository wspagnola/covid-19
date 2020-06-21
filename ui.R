library(shiny)
library(shinycustomloader)
library(tidyverse)
library(shinythemes)
library(plotly)


source("data_processing.R")

navbarPage(
  
  theme = shinytheme('flatly'),
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
           withLoader(plotlyOutput("cases_country")),
           withLoader(plotlyOutput('deaths_country'))
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
            withLoader(plotlyOutput("state_cases")),
            withLoader(plotlyOutput("state_deaths"))
        
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
      
    withLoader(plotlyOutput("county_cases")),
    withLoader(plotlyOutput("county_deaths"))
  ),

  collapsible = TRUE
  
)