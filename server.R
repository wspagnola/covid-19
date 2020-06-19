library(shiny)
library(shinycustomloader)
library(tidyverse)

source("data_processing.R")

server <- function(input, output, session){
  
  #### Notes####
  
    # Possible Improvements
    # 1) Look at inconsistencies in cumulative data 
    # (e.g. when later date cases/deaths lower than earlier dates)
    # 2) Add tooltip to hover over plots and provide stats for specific dates (w/ shinyBS package?)
    # 3) Develop deeper understanding reactivity to find ways to reduce code redundancy
    # 4) Evaluate data_processing.R script to reduce load time
    # 5) Adjust interdependent controls w/ update button (Maybe?)
    # 6) Add loading spinner 
  
  #### Country by Continent Input ####
  
  #Code for Interdependent Selection borrowed from Charles Hadley LinkedIn Learning Chpt. 06 Ex. 03
  
  updateSelectInput(session,
                    'continent',
                    choices = unique(countries$continent))

  observeEvent(c(input$continent),
               {
                  countries_in_continent <- countries %>%
                                              filter(continent == input$continent) %>%
                                              pull(country)
                  updateSelectInput(session,
                                    'country',
                                    choices = unique(countries_in_continent)
                  )
               }
   )
  
    # Save indicator input as reactive expression
    country_indicator <- reactive({input$indicator_country})
  
    #### Country Plots ####
  
    output$cases_country <- renderPlot({
      
               countries_plot <-  countries %>% 
                                  filter(country == input$country)  %>% 
                                  filter(cases > 0 ) %>% 
                                  ggplot() +
                                   theme_bw() +
                                   main_theme +
                                   xlab('') +
                                   ylab('')
      
              if (country_indicator() == "New Cases/Deaths"){
                
                      # Countries New Cases Plot 
                      countries_plot  +
                        geom_col(aes(x=date, y=new_cases, fill = weekend)) +
                        labs(title = paste0(input$country, ": New Cases"),
                             caption = "Source: Johns Hopkins")
                      
              } else if (country_indicator() == "Cumulative Cases/Deaths (Linear)"){
      
                      # Countries Cases Plot
                      countries_plot + 
                          geom_line(aes(x=date, y=cases)) +
                          geom_point(aes(x=date, y=cases)) +
                          scale_y_continuous(labels = scales::comma) +
                          labs(title = paste0(input$country, ": Cumulative Cases (Linear)"),
                               caption = "Source: Johns Hopkins") 
      
              } else if(country_indicator() ==  "Cumulative Cases/Deaths (Logistic)") {
                
                      # Countries Cases Plot
                      countries_plot + 
                      geom_line(aes(x=date, y=cases)) +
                        geom_point(aes(x=date, y=cases)) + 
                            scale_y_log10(labels = scales::comma) +
                            theme_bw() +
                            main_theme + 
                            labs(title = paste0(input$country, ": Cumulative Cases (Logistic)"),
                                 caption = "Source: Johns Hopkins") 
              }
    }) 
  
    output$deaths_country <- renderPlot({
      
                    countries_plot <-  countries %>% 
                      filter(country == input$country)  %>% 
                      filter(cases > 0 ) %>% 
                      ggplot() +
                      theme_bw() +
                      main_theme +
                      xlab('') +
                      ylab('')
    
      if (country_indicator() == "New Cases/Deaths"){
                    
                    # Country Cases Plot 
                    countries_plot +
                      geom_col(aes(x=date, y=new_deaths, fill = weekend)) +
                      labs(title = paste0(input$country, ": New Deaths"),
                           caption = "Source: Johns Hopkins")
        
        
      } else if (country_indicator() == "Cumulative Cases/Deaths (Linear)"){
        
                  # Country Deaths Plot 
                  countries_plot  +
                      geom_line(aes(x=date, y=deaths)) +
                      geom_point(aes(x=date, y=deaths)) +
                      scale_y_continuous(labels = scales::comma) +
                      labs(title = paste0(input$country, ": Cumulative Deaths (Linear)"),
                           caption = "Source: Johns Hopkins") 
        
      } else if(country_indicator() ==  "Cumulative Cases/Deaths (Logistic)") {
        
        #Create vector to scale dates on x-axis
        date_vec <- countries %>%  
                        filter(country == input$country, 
                               cases > 0) %>% 
                        pull(date)
     
        countries %>%  
          filter(country == input$country,) %>% 
          filter(deaths > 0) %>% 
          ggplot(aes(x=date, y=deaths)) +
          geom_line() +
          geom_point() +
          scale_x_date(limits = c(min(date_vec), max(date_vec)  )) +
          scale_y_log10(labels = scales::comma) +
          theme_bw() +
          main_theme + 
          labs(title = paste0(input$country, ": Cumulative Deaths (Logistic)"),
               caption = "Source: Johns Hopkins") 
        
        
      }
  }) 
  
  
    #### State Plots ####
    
      state_indicator <-reactive({input$indicator_state})
  
        output$state_cases <- renderPlot({
          
                     states_plot <- states %>% 
                                      filter(state == input$state)  %>% 
                                     #filter(!is.na(new_cases)) %>%#
                                      ggplot() +
                                         theme_bw() +
                                         main_theme +
                                         xlab('') +
                                         ylab('') 
                    
                          if(state_indicator() == "New Cases/Deaths"){
                            
                                  states_plot + 
                                      geom_col(aes(x=date, y=new_cases, fill = weekend)) +
                                      labs(title = paste0(input$state, ": New Cases"),
                                          caption = "Source: New York Times")
                                
                            
                          } else if(state_indicator() == "Cumulative Cases/Deaths (Linear)"){
                            
                                #Note: Not sure if I should filter out dates before first case
                                states_plot +
                                  geom_line(aes(x=date, y=cases)) +
                                  geom_point(aes(x=date, y=cases)) +
                                  scale_y_continuous(labels = scales::comma) +
                                  labs(title = paste0(input$state, ": Cumulative Cases (Linear)"),
                                       caption = "Source: New York Times")
                                
                            
                          } else if(state_indicator()==  "Cumulative Cases/Deaths (Logistic)") {
                            
                                  states_plot +
                                    geom_line(aes(x=date, y=cases)) +
                                    geom_point(aes(x=date, y=cases)) +
                                    scale_y_log10(labels = scales::comma) + 
                                    labs(title = paste0(input$state, ": Cumulative Cases (Logistic)"),
                                         caption = "Source: New York Times")
                          }
                        
        })
        
        output$state_deaths <- renderPlot({
          
                        #Note: this may be redundant
                        states_plot <- states %>% 
                          filter(state == input$state)  %>% 
                          ggplot() +
                          theme_bw() +
                          main_theme +
                          xlab('') +
                          ylab('') 
          
                  if (state_indicator() ==  "New Cases/Deaths") {
                    
                        states_plot +
                        geom_col(aes(x=date, y=new_cases, fill = weekend)) +
                        labs(title = paste0(input$state, ": New Deaths"),
                             caption = "Source: New York Times")
                    
                    
                  } else if(state_indicator()==  "Cumulative Cases/Deaths (Linear)") {

                      states_plot +
                          geom_line(aes(x=date, y=deaths)) +
                          geom_point(aes(x=date, y=deaths)) +
                          scale_y_continuous(labels = scales::comma) +
                          labs(title = paste0(input$state, ": Cumulative Deaths (Linear)"),
                               caption = "Source: New York Times")
                      
                  } else if(state_indicator()==  "Cumulative Cases/Deaths (Logistic)") {
                    
                        # Note: Use different code in order to keep x-axis limits same as cases plot
                        #Create vector to scale dates on x-axis
                        date_vec <- states %>%  
                          filter(state == input$state, 
                                 cases > 0) %>% 
                          pull(date)
                    
                        states %>% 
                          filter(state == input$state)  %>% 
                          filter(deaths > 0) %>% 
                          ggplot(aes(x=date, y=deaths)) +
                          geom_line() +
                          geom_point() +
                          scale_x_date(limits = c(min(date_vec), max(date_vec))) +
                          scale_y_log10(labels = scales::comma) + 
                          theme_bw() +
                          main_theme +                
                          xlab('') +
                          ylab('') + 
                          labs(title = paste0(input$state, ": Cumulative Deaths (Logistic)"),
                               caption = "Source: New York Times")
                    
                  }
                  
        })
        
        
        #### County Plots ####
        
        
        updateSelectInput(session, 'state_filter', choices = unique(counties$state))
        
        observeEvent(c(input$state_filter),
                     {
                       counties_in_state <- counties %>%
                                               filter(state== input$state_filter) %>%
                                               pull(county)
                       
                       updateSelectInput(session, 'county',  choices = unique(counties_in_state))
                     }
        )
        
        
        county_indicator <-reactive({input$indicator_county})
        
        output$county_cases <- renderPlot({
          
          
          counties_plot <- counties %>% 
                            filter(county == input$county, state== input$state_filter)  %>% 
                            ggplot() +
                              theme_bw() +
                              main_theme +
                              xlab('') +
                              ylab('')
          
          if(county_indicator() == "New Cases/Deaths"){
    
                          counties_plot +
                            geom_col(aes(x=date, y=new_cases, fill = weekend)) +
                            labs(title = paste0(input$county, ": New Cases"),
                                 caption = "Source: New York Times")
            
            
          } else if(county_indicator() == "Cumulative Cases/Deaths (Linear)"){
            
                    # County Cases 
                    counties_plot +
                      geom_line(aes(x=date, y=cases)) +
                      geom_point(aes(x=date, y=cases)) +
                      scale_y_continuous(labels = scales::comma) +
                      labs(title = paste0(input$county, ": Cumulative Cases (Linear)"),
                           caption = "Source: New York Times")
            
            
          } else if(county_indicator()==  "Cumulative Cases/Deaths (Logistic)") {
            
                    counties_plot +
                      geom_line(aes(x=date, y=cases)) +
                      geom_point(aes(x=date, y=cases)) +
                      scale_y_log10(labels = scales::comma) + 
                      labs(title = paste0(input$county, ": Cumulative Cases (Logistic)"),
                           caption = "Source: New York Times")
            
          }
        })
        
        output$county_deaths <- renderPlot({
          
                #Note: May be Redundant
                counties_plot <- counties %>% 
                  filter(county == input$county, state== input$state_filter)  %>% 
                  ggplot() +
                  theme_bw() +
                  main_theme +
                  xlab('') +
                  ylab('')
          
          if (county_indicator() ==  "New Cases/Deaths") {
            
                  #New County Deaths 
                  counties_plot +
                    geom_col(aes(x=date, y=new_deaths, fill = weekend)) +
                    labs(title = paste0(input$county, ": New Deaths"),
                         caption = "Source: New York Times")
                  
            
          } else if(county_indicator()==  "Cumulative Cases/Deaths (Linear)") {
            
                    
                    # County Deaths
                    counties_plot +
                      geom_line(aes(x=date, y=deaths)) +
                      geom_point(aes(x=date, y=deaths)) +
                      scale_y_continuous(labels = scales::comma) +
                      labs(title = paste0(input$county, ": Cumulative Daths (Linear)"),
                           caption = "Source: New York Times")
            
          } else if(county_indicator()==  "Cumulative Cases/Deaths (Logistic)") {
            
            #Create vector to scale dates on x-axis
            date_vec <- counties %>%  
              filter(county == input$county, state== input$state_filter, cases > 0) %>% 
              pull(date)
            
            counties %>% 
              filter(county == input$county, state== input$state_filter)  %>% 
              filter(deaths > 0) %>% 
              ggplot(aes(x=date, y=deaths)) +
              geom_line() +
              geom_point() +
              scale_x_date(limits = c(min(date_vec), max(date_vec))) +
              scale_y_log10(labels = scales::comma) + 
              theme_bw() +
              main_theme +                
              xlab('') +
              ylab('') + 
              labs(title = paste0(input$county, ": Cumulative Deaths (Logistic)"),
                   caption = "Source: New York Times")
          }
        })
}