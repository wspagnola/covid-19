library(shiny)
library(shinycustomloader)
library(tidyverse)
library(shinythemes)
library(profvis)
library(plotly)
library(lubridate)

source("data_processing.R")


#Vector of plotly toolbar items to remove 
remove_toolbar_vec <- c("sendDataToCloud","editInChartStudio", "zoom2d", "zoomIn2d", "zoomOut2d", 
                        "pan2d","select2d", "lasso2d", "drawclosedpath", "drawopenpath",
                        "drawline", "drawrect", "drawcircle", "eraseshape", "autoScale2d",
                        "resetScale2d", "hoverClosestCartesian", "hoverCompareCartesian",
                        "toggleSpikelines")

server <- function(input, output, session){
  
    #### Notes####
    
      # Possible Improvements
      # 1) Look at inconsistencies in cumulative data 
      # (e.g. when later date cases/deaths lower than earlier dates)
      # 2) Develop deeper understanding reactivity to find ways to reduce code redundancy
      # 3) Evaluate data_processing.R script to reduce load time
      # 4) Use validate to return message if no data within selected location/date range
      # 5) Add captions with name of data source in plotly::layout() function call
  
    #### Country by Continent Input ####
    
    #Code for Interdependent Selection borrowed from Charles Hadley LinkedIn Learning Chpt. 06 Ex. 03
    
    
    #Note: not sure how to make USA default upon opening
    #country_default <- reactiveVal('United States of America')
    
  
    country_counter <- reactiveVal(0)
  
    updateSelectInput(session,
                      'continent',
                      choices = unique(countries$continent),
                      selected = 'Americas')
  
    observeEvent(c(input$continent),
        {
            countries_in_continent <- countries %>%
                                        filter(continent == input$continent) %>%
                                        pull(country)
            
            current_state <- country_counter()
            
            #Not sure why this Event Occurs twice before first graph 
            if(current_state < 2 ){
              default_country <- 'United States of America'
              
            } else{
              
              default_country <- NULL
            }
            
            updateSelectInput(session,
                              'country',
                              choices = unique(countries_in_continent),
                              selected = default_country
            
            )
  
            new_value <- country_counter() + 1
            country_counter(new_value)
            
            print(paste('Country Counter:', new_value))
            
    })
  
    # Save indicator input as reactive expression
    country_indicator <- reactive({input$indicator_country})
    
    rv_country <- reactiveValues(baseplot =  NULL)
    
    observeEvent(c(input$update_country_chart), 
                 
        {
      
          print('Update Country Chart')

          start_date <- input$country_dates[1]
          end_date <- input$country_dates[2]
          
           rv_country$baseplot  <- countries %>% 
                                    filter(country == input$country)  %>%
                                    filter(between(date, start_date, end_date)) %>% 
                                    ggplot() +
                                    theme_bw() +
                                    main_theme +
                                    xlab('') +
                                    ylab('')
                
           if (country_indicator() ==  "Cumulative Cases/Deaths (Logistic)"){
             
              # Maybe should use an if statement here
              date_vec <- countries %>%  
                              filter(country ==  input$country) %>% 
                              filter(between(date, start_date, end_date)) %>% 
                              filter(cases > 0) %>% 
                              pull(date)
 
                
              rv_country$log_death_plot <- countries %>%
                                              filter(country == input$country,) %>%
                                              filter(between(date, start_date, end_date)) %>%
                                              filter(deaths > 0) %>%
                                              ggplot(aes(x=date, y=deaths)) +
                                                  geom_line() +
                                                  geom_point() +
                                                  scale_x_date(limits = c(min(date_vec), max(date_vec))) +
                                                  scale_y_log10(labels = scales::comma) +
                                                  theme_bw() +
                                                  main_theme +
                                                  labs(title = paste0(input$country, ": Cumulative Deaths (Logistic)"),
                                                       caption = "Source: Johns Hopkins")
            }
    })

    #### Country Plots ####
  
    output$cases_country <- renderPlotly({
      
          if(input$update_country_chart == 0 ){

            return()
          }
          
  
          countries_plot <-  rv_country$baseplot
  
          ggplotly(
              
            if (isolate(country_indicator() )== "New Cases/Deaths"){
              
                    # Countries New Cases Plot 
                     countries_plot +
                        geom_col(aes(x=date, y=new_cases, fill = weekend)) +
                        theme(legend.position="top") +
                        labs(title = paste0(isolate(input$country), ": New Cases"),
                             caption = "Source: Johns Hopkins") 
                    
            } else if (isolate(country_indicator()) == "Cumulative Cases/Deaths (Linear)"){
    
                    # Countries Cases Plot
                    countries_plot + 
                        geom_line(aes(x=date, y=cases)) +
                        geom_point(aes(x=date, y=cases)) +
                        scale_y_continuous(labels = scales::comma) +
                        labs(title = paste0(isolate(input$country), ": Cumulative Cases (Linear)"),
                             caption = "Source: Johns Hopkins") 
    
            } else if(isolate(country_indicator()) ==  "Cumulative Cases/Deaths (Logistic)") {
              
                    # Countries Cases Plot
                    countries_plot  + 
                    geom_line(aes(x=date, y=cases)) +
                      geom_point(aes(x=date, y=cases)) + 
                      scale_y_log10(labels = scales::comma) +
                      theme_bw() +
                      main_theme + 
                      labs(title = paste0(isolate(input$country), ": Cumulative Cases (Logistic)"),
                               caption = "Source: Johns Hopkins") 
            }, 
          
          tooltip = c('x', 'y')) %>% 
          layout(hoverlabel=list(bgcolor="white"),
                 legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1)) %>% 
            config(modeBarButtonsToRemove = remove_toolbar_vec, displaylogo = FALSE)
          
    }) 
  
    output$deaths_country <- renderPlotly({
      
      
      if(input$update_country_chart == 0 ){
        
        return()
      }
       countries_plot <- rv_country$baseplot
      
       ggplotly(
         
              if (isolate(country_indicator()) == "New Cases/Deaths"){
                            
                            # Country Deaths Plot 
                            countries_plot +
                              geom_col(aes(x=date, y=new_deaths, fill = weekend)) +
                              labs(title = paste0(isolate(input$country), ": New Deaths"),
                                   caption = "Source: Johns Hopkins")
                
              } else if (isolate(country_indicator()) == "Cumulative Cases/Deaths (Linear)"){
                
                          # Country Deaths Plot 
                          countries_plot +
                          geom_line(aes(x=date, y=deaths)) +
                          geom_point(aes(x=date, y=deaths)) +
                          scale_y_continuous(labels = scales::comma) +
                          labs(title = paste0(isolate(input$country), ": Cumulative Deaths (Linear)"),
                               caption = "Source: Johns Hopkins") 
                
              } else if(isolate(country_indicator()) == "Cumulative Cases/Deaths (Logistic)") {
                
                #Cumulative Deaths Logistic 
                death_plot <- rv_country$log_death_plot
                death_plot 

              },
      
      tooltip = c('x', 'y') ) %>%
      layout(hoverlabel=list(bgcolor="white"),
                legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1)) %>% 
      config(modeBarButtonsToRemove = remove_toolbar_vec, displaylogo = FALSE)
       
  }) 
  
  #### State Plots ####
  
  state_indicator <-reactive({input$indicator_state})
  
  rv_state <- reactiveValues(baseplot =  NULL)
    
  observeEvent(c(input$update_state_chart), 
      {
        
        start_date <- input$state_dates[1]
        end_date <- input$state_dates[2]
        
        rv_state$baseplot <- states %>% 
                                filter(state == input$state)  %>% 
                                filter(between(date, start_date, end_date)) %>% 
                                filter(cases > 0 ) %>% 
                                ggplot() +
                                  theme_bw() +
                                  main_theme +
                                  xlab('') +
                                  ylab('') 
          
        if (state_indicator() ==  "Cumulative Cases/Deaths (Logistic)"){
            
            
            # Note: Use different code in order to keep x-axis limits same as cases plot
            #Create vector to scale dates on x-axis
            date_vec <- states %>%  
                          filter(state == input$state) %>% 
                          filter(between(date, start_date, end_date)) %>% 
                          filter(cases > 0) %>% 
                          pull(date)

             rv_state$log_death_plot <-  states %>% 
                                            filter(state == input$state)  %>% 
                                            filter(between(date, start_date, end_date)) %>% 
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
  
      output$state_cases <- renderPlotly({
        
              if(input$update_state_chart == 0){
                
                return()
                
              }
          
              states_plot <- rv_state$baseplot
              
              indicator_state <- isolate(state_indicator())
        
              ggplotly(
                          if(indicator_state == "New Cases/Deaths"){
                            

                                  states_plot + 
                                      geom_col(aes(x=date, y=new_cases, fill = weekend)) +
                                      labs(title = paste0(isolate(input$state), ": New Cases"),
                                          caption = "Source: New York Times")
                                  
                          } else if(indicator_state == "Cumulative Cases/Deaths (Linear)"){
                            
                                states_plot +
                                  geom_line(aes(x=date, y=cases)) +
                                  geom_point(aes(x=date, y=cases)) +
                                  scale_y_continuous(labels = scales::comma) +
                                  labs(title = paste0(isolate(input$state), ": Cumulative Cases (Linear)"),
                                       caption = "Source: New York Times")
                                
                          } else if(indicator_state ==  "Cumulative Cases/Deaths (Logistic)") {
                               
                                  states_plot +
                                    geom_line(aes(x=date, y=cases)) +
                                    geom_point(aes(x=date, y=cases)) +
                                    scale_y_log10(labels = scales::comma) + 
                                    labs(title = paste0(isolate(input$state), ": Cumulative Cases (Logistic)"),
                                         caption = "Source: New York Times")
                            
                          },
              
              tooltip = c('x', 'y')) %>% 
              layout(hoverlabel=list(bgcolor="white"),
                       legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1)) %>%
                config(modeBarButtonsToRemove = remove_toolbar_vec, displaylogo = FALSE)
              
        })
        
        output$state_deaths <- renderPlotly({
          
            if(input$update_state_chart == 0){
              
              return()
              
            }
          
            states_plot <- rv_state$baseplot
            
            indicator_state <- isolate(state_indicator())
          
            ggplotly(
              
                  if (indicator_state ==  "New Cases/Deaths") {

                         #States: Cumulative New Deaths
                        states_plot +
                        geom_col(aes(x=date, y=new_cases, fill = weekend)) +
                        labs(title = paste0(isolate(input$state), ": New Deaths"),
                             caption = "Source: New York Times")
   
                  } else if(indicator_state ==  "Cumulative Cases/Deaths (Linear)") {

                        #States: Cumulative Deaths (Linear)
                        states_plot +
                            geom_line(aes(x=date, y=deaths)) +
                            geom_point(aes(x=date, y=deaths)) +
                            scale_y_continuous(labels = scales::comma) +
                            labs(title = paste0(isolate(input$state), ": Cumulative Deaths (Linear)"),
                                 caption = "Source: New York Times")
                    
                      
                  } else if(indicator_state ==  "Cumulative Cases/Deaths (Logistic)") {
                    
                           #States: Cumulative Deaths (Logistic)
                          death_plot <- rv_state$log_death_plot
                          death_plot 
                  },
                  
          tooltip = c('x', 'y')) %>% 
          layout(hoverlabel=list(bgcolor="white"),
                     legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1)) %>% 
          config(modeBarButtonsToRemove = remove_toolbar_vec, displaylogo = FALSE)
        })
        
        
        #### County Plots ####
        
        county_counter <- reactiveVal(0)
        
        updateSelectInput(session, 
                          'state_filter', 
                          choices = unique(counties$state),
                          selected = 'New York')
        
        observeEvent(c(input$state_filter),
            {
                       
                 # Note: Maybe change state; can be confused w/ geographic location rather than 'setting'
                 current_state <- county_counter()
                 

                 counties_in_state <- counties %>%
                                         filter(state== input$state_filter) %>%
                                         pull(county)
                 

                 
                 #Not sure why this Event Occurs twice before first graph 
                 if(current_state < 2 ){
                   
                   default_county <- 'New York City' 
                   
                 } else{
                   
                   default_county <- NULL
                 }
                 
                 updateSelectInput(session, 
                                   'county',  
                                   choices = unique(counties_in_state),
                                   selected = default_county)
              
               
               new_value <- county_counter() + 1
               county_counter(new_value)
               print(paste('County Counter:', new_value))
                     
      })
        
        
      #Note: Not sure why if statement in next observeEvent call needs to be 
      county_indicator <-reactive({input$indicator_county})
      
       rv_counties <- reactiveValues(
          baseplot =  NULL
         
       )
        
        observeEvent((input$update_county_chart), {
          
            start_date <- input$county_dates[1]
            end_date <- input$county_dates[2]
            
            rv_counties$baseplot  <- counties %>% 
                                        filter(county == input$county, state== input$state_filter )  %>%
                                        filter(between(date, start_date, end_date)) %>% 
                                        filter(cases > 0 ) %>% 
                                        ggplot() +
                                            theme_bw() +
                                            main_theme +
                                            xlab('') +
                                            ylab('')
            
            if (county_indicator() ==  "Cumulative Cases/Deaths (Logistic)"){
                  
                #Create vector to scale dates on x-axis
                date_vec <- counties %>%  
                  filter(county == input$county, state== input$state_filter) %>% 
                  filter(between(date, start_date, end_date)) %>% 
                  filter(cases > 0) %>% 
                  pull(date)
                
                
                rv_counties$log_death_plot <- counties %>% 
                                                  filter(county == input$county, state == input$state_filter)  %>% 
                                                  filter(between(date, start_date, end_date)) %>% 
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
        
      output$county_cases <- renderPlotly({
        
          print(input$update_county_chart)
        
          #NOTE: not sure if I want to return nothing here          
          if(input$update_county_chart == 0 ){
            
            return()
          }
        
          counties_plot <-  rv_counties$baseplot

         ggplotly(
          
            if(isolate(input$indicator_county) == "New Cases/Deaths"){
               
               # County New Cases 
                counties_plot +
                  geom_col(aes(x=date, y=new_cases, fill = weekend)) +
                  labs(title = paste0(isolate(input$county), ": New Cases"),
                       caption = "Source: New York Times")

            } else if(isolate(input$indicator_county)== "Cumulative Cases/Deaths (Linear)"){
          
                # County Cases (Linear)
                counties_plot +
                  geom_line(aes(x=date, y=cases)) +
                  geom_point(aes(x=date, y=cases)) +
                  scale_y_continuous(labels = scales::comma) +
                  labs(title = paste0(isolate(input$county), ": Cumulative Cases (Linear)"),
                       caption = "Source: New York Times")
         
            } else if(isolate(input$indicator_county) ==  "Cumulative Cases/Deaths (Logistic)") {
          
                # County Cases (Logistic)
                counties_plot +
                  geom_line(aes(x=date, y=cases)) +
                  geom_point(aes(x=date, y=cases)) +
                  scale_y_log10(labels = scales::comma) + 
                  labs(title = paste0(isolate(input$county), ": Cumulative Cases (Logistic)"),
                       caption = "Source: New York Times")
        },
        
        tooltip = c('x', 'y')) %>% 
        layout(hoverlabel=list(bgcolor="white"),
                  legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1)) %>%
        config(modeBarButtonsToRemove = remove_toolbar_vec, displaylogo = FALSE)
      })
      
      output$county_deaths <- renderPlotly({
        
        if(input$update_county_chart == 0 ){
          
          return()
        }
        
        counties_plot <-  rv_counties$baseplot
        
        ggplotly(
          if (isolate(input$indicator_county) ==  "New Cases/Deaths") {
            
                  #New County Deaths 
                    counties_plot +
                      geom_col(aes(x=date, y=new_deaths, fill = weekend)) +
                      labs(title = paste0(isolate(input$county), ": New Deaths"),
                           caption = "Source: New York Times")
                  
          } else if(isolate(input$indicator_county) ==  "Cumulative Cases/Deaths (Linear)") {
            
                    # County Deaths (Linear)
                      counties_plot +
                        geom_line(aes(x=date, y=deaths)) +
                        geom_point(aes(x=date, y=deaths)) +
                        scale_y_continuous(labels = scales::comma) +
                        labs(title = paste0(isolate(input$county), ": Cumulative Deaths (Linear)"),
                             caption = "Source: New York Times")
            
          } else if(isolate(input$indicator_county) ==  "Cumulative Cases/Deaths (Logistic)") {
            
                    # County Deaths (Logistic) S
                    death_plot <-   rv_counties$log_death_plot 
                    death_plot
            
          },
          
          tooltip = c('x', 'y')) %>%
          layout(hoverlabel=list(bgcolor="white"),
                 legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1)) %>%
          config(modeBarButtonsToRemove = remove_toolbar_vec, displaylogo = FALSE)
      })
}

#profvis(runApp())