library(shiny)
library(shinycustomloader)
library(tidyverse)
library(shinythemes)
library(plotly)
library(lubridate)


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
          ),
   
            fluidRow(column(
            dateRangeInput('country_dates', 'Dates', start = "2019-12-01", end =  today(),
                           min = "2019-12-01", max = today()),
                width = 6)
            
          )),

          actionButton('update_country_chart', label = 'Update Chart', width = '100%'),
           withLoader(plotlyOutput("cases_country")),
           withLoader(plotlyOutput('deaths_country')),
          
          fluidPage(fluidRow(
                    column(width=8),
                    column('Data Source: Johns Hopkins University', width = 4))
          )
    ),
 
  tabPanel("States",
         
           
      fluidPage(fluidRow(
              column(selectInput("state",
                                label = "State", 
                                choices = unique(states$state),
                                selected = 'New York'),
                      width = 6),
              column(selectInput('indicator_state',
                                 label = 'Select Indicator',
                                 choices = c("New Cases/Deaths",
                                              "Cumulative Cases/Deaths (Linear)",
                                              "Cumulative Cases/Deaths (Logistic)")),
                     width = 6)
        ),
              fluidRow(
                  column(dateRangeInput('state_dates', 'Dates', 
                                        start = "2019-12-01", end =  today(),
                                        min = "2019-12-01", max = today()),
                    width = 6)
        
      )),
        

      actionButton('update_state_chart', label = 'Update Chart', width = '100%'),
      withLoader(plotlyOutput("state_cases")),
      withLoader(plotlyOutput("state_deaths")),
      fluidPage(fluidRow(
                          column(width=8),
                          column('Data Source: New York Times', width = 4)
      ))
        
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
      ),
              fluidRow(
                column(dateRangeInput('county_dates', 'Dates', 
                                      start = "2019-12-01", end =  today(),
                                      min = "2019-12-01", max = today()),
                       width = 6)
        
      )), 
      
    actionButton('update_county_chart', label = 'Update Chart', width = '100%'), 
    withLoader(plotlyOutput("county_cases")),
    withLoader(plotlyOutput("county_deaths")),
    fluidPage(fluidRow(
      column(width=8),
      column('Data Source: New York Times', width = 4)
    ))
  ),

  collapsible = TRUE
  
)