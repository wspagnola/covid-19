library(shiny)
library(shinycustomloader)
library(tidyverse)

source("data_processing.R")

server <- function(input, output, session){
  
  

  
  #### Notes ####
  
  # 1) need to adjust geom_() depending on selected indicator
  # 2) need to adjust y argument for geom_() call depending on selected indicator
  # 3) need to add argument to scale_y_continous for logistic scale 
  
  
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
      
      
              if (country_indicator() == "New Cases/Deaths"){
                
                #Note: should I fill bars by weekend? 
                
                countries %>% 
                  filter(country == input$country)  %>% 
                  filter(new_cases > 0 ) %>%
                  ggplot(aes(x=date, y=new_cases, fill = weekend)) +
                  geom_col() +
                  theme_bw() +
                  main_theme +
                  xlab('') +
                  ylab('') + 
                  labs(title = paste0(input$country, ": New Cases"),
                       caption = "Source: Johns Hopkins")
                
                
                
                
              } else if (country_indicator() == "Cumulative Cases/Deaths (Linear)"){
      
                  countries %>%  
                    filter(country == input$country,) %>% 
                    filter(cases > 0) %>% 
                    ggplot(aes(x=date, y=cases)) +
                    geom_line() +
                    geom_point() +
                    scale_y_continuous(labels = scales::comma) +
                    theme_bw() +
                    main_theme + 
                    labs(title = paste0(input$country, ": Cumulative Cases (Linear)"),
                         caption = "Source: Johns Hopkins") 
      
              } else if(country_indicator() ==  "Cumulative Cases/Deaths (Logistic)") {
                
      
                
                    countries %>%  
                      filter(country == input$country) %>% 
                      filter(cases > 0) %>% 
                      ggplot(aes(x=date, y=cases)) +
                      geom_line() +
                      geom_point() +
                      scale_y_log10(labels = scales::comma) +
                      theme_bw() +
                      main_theme + 
                      labs(title = paste0(input$country, ": Cumulative Cases (Logistic)"),
                           caption = "Source: Johns Hopkins") 
                
                
              }
        

    }) 
  
    output$deaths_country <- renderPlot({
    
      if (country_indicator() == "New Cases/Deaths"){
        
        countries %>% 
          filter(country == input$country)  %>% 
          filter(new_cases > 0) %>% 
          ggplot(aes(x=date, y=new_deaths, fill = weekend)) +
          geom_col() +
          theme_bw() +
          main_theme +
          xlab('') +
          ylab('') + 
          labs(title = paste0(input$country, ": New Cases"),
               caption = "Source: Johns Hopkins")
        
        
      } else if (country_indicator() == "Cumulative Cases/Deaths (Linear)"){
        
        countries %>%  
          filter(country == input$country,) %>% 
          filter(cases > 0) %>% 
          ggplot(aes(x=date, y=deaths)) +
          geom_line() +
          geom_point() +
          scale_y_continuous(labels = scales::comma) +
          theme_bw() +
          main_theme + 
          labs(title = paste0(input$country, ": Cumulative Cases (Linear)"),
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
          
                         
                          if(state_indicator() == "New Cases/Deaths"){
                            
                            states %>% 
                              filter(state == input$state)  %>% 
                              filter(!is.na(new_cases)) %>%
                              ggplot(aes(x=date, y=new_cases, fill = weekend)) +
                              geom_col() +
                              theme_bw() +
                              main_theme +
                              xlab('') +
                              ylab('') + 
                              labs(title = paste0(input$state, ": New Cases"),
                                   caption = "Source: New York Times")
                            
                            
                          } else if(state_indicator() == "Cumulative Cases/Deaths (Linear)"){
                            
                            #Note: Not sure if I should filter out dates before first case
                            states %>% 
                              filter(state == input$state)  %>% 
                              filter(cases > 0 ) %>%
                              ggplot(aes(x=date, y=cases)) +
                              geom_line() +
                              geom_point() +
                              scale_y_continuous(labels = scales::comma) +
                              theme_bw() +
                              main_theme +
                              xlab('') +
                              ylab('') +
                              labs(title = paste0(input$state, ": Cumulative Cases (Linear)"),
                                   caption = "Source: New York Times")
                            
                            
                          } else if(state_indicator()==  "Cumulative Cases/Deaths (Logistic)") {
                            
                            states %>% 
                              filter(state == input$state)  %>% 
                              filter(cases > 0) %>% 
                              ggplot(aes(x=date, y=cases)) +
                              geom_line() +
                              geom_point() +
                              scale_y_log10(labels = scales::comma) + 
                              theme_bw() +
                              main_theme +
                              xlab('') +
                              ylab('') +
                              labs(title = paste0(input$state, ": Cumulative Cases (Logistic)"),
                                   caption = "Source: New York Times")
                            
                          }
                        
        })
        
        output$state_deaths <- renderPlot({
          
                  if (state_indicator() ==  "New Cases/Deaths") {
                    
                      states %>% 
                        filter(state == input$state)  %>% 
                        filter(!is.na(new_cases)) %>%
                        ggplot(aes(x=date, y=new_cases, fill = weekend)) +
                        geom_col() +
                        theme_bw() +
                        main_theme +
                        xlab('') +
                        ylab('') + 
                        labs(title = paste0(input$state, ": New Deaths"),
                             caption = "Source: New York Times")
                    
                    
                  } else if(state_indicator()==  "Cumulative Cases/Deaths (Linear)") {

                        states %>% 
                          filter(state == input$state)  %>% 
                          ggplot(aes(x=date, y=deaths)) +
                          geom_line() +
                          geom_point() +
                          scale_y_continuous(labels = scales::comma) +
                          theme_bw() +
                          main_theme +
                          xlab('') + 
                          ylab('') + 
                          labs(title = paste0(input$state, ": Cumulative Deaths (Linear)"),
                               caption = "Source: New York Times")
                      
                  } else if(state_indicator()==  "Cumulative Cases/Deaths (Logistic)") {
                    
                    
                    
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

}