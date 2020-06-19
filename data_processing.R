
library(tidyverse)
# Note replace NA new_cases with 1?

#### States  ####

url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'

state_data <- read_csv(url)

states <-state_data %>% 
  arrange(state, date) %>% 
  group_by(state) %>% 
  mutate( new_cases= cases - lag(cases),
          new_deaths = deaths - lag(deaths)
)

#Recode NA from first date as equal to cumulative variable
states <- states %>% 
            mutate(new_cases = if_else(is.na(new_cases), cases, new_cases),
                   new_deaths = if_else(is.na(new_deaths), deaths, new_deaths),
                   weekdays = weekdays(date),
                   weekend = ifelse(weekdays %in% c('Saturday', 'Sunday'), 'Weekend', "Weekday"),
                   weekend = as.factor(weekend)
)


#### Countries ####


#Load cases
url <-'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
confirmed_cases <- read_csv(url)

#Load deaths 
url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
deaths <- read_csv(url)


## NOTE: summarise()` regrouping output by 'country' (override with `.groups` argument)
# It's weird because some countries are disaggregated by region/state/province

# Plot Confirmed Cases by Country
cases_long <- confirmed_cases %>% 
  select(-Lat, -Long, -`Province/State`) %>% 
  rename(country = `Country/Region`) %>% 
  gather(key = 'date', value = 'cases', -country) %>% 
  group_by(country, date) %>% 
  summarize(cases = sum(cases)) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y")) 

## NOTE: summarise()` regrouping output by 'country' (override with `.groups` argument)
# It's weird because some countries are disaggregated by region/state/province

#Convert to Long 
deaths_long <- deaths  %>%
  select(-Lat, -Long, -`Province/State`) %>%
  rename(country = `Country/Region`) %>%
  gather(key = 'date', value = 'deaths', - country) %>%
  group_by(country, date) %>%
  summarize(deaths = sum(deaths)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))



#Merge Data and then Convert to Long 
full_long <- cases_long %>% 
  full_join(deaths_long, by = c('country', 'date')) 


#Note: Not sure about this structure
#full_long_long <- full_long %>% gather(key, value, cases:deaths) 

full_long_clean <- full_long %>% 
                     mutate(country = str_remove(country, "\\*" )) %>% 
                     mutate( country = case_when(
                      country == 'Korea, South' ~ 'South Korea',
                     country == "Burma" ~ "Myanmar",
                     country ==  'Congo (Brazzaville)' ~ 'Congo',  
                     country == "US" ~ "United States of America",
                     country ==  'Congo (Kinshasa)' ~ 'Democratic Republic of the Congo',
                     TRUE ~ country
))

continents_clean <- read_csv('data/continents_clean.csv')

countries <- full_long_clean %>% 
             left_join(continents_clean, by = c('country'))   


#CHECK UNMATCHED COUNTRIES
# unmatched_countries <- countries %>%  
#                           filter(is.na(continent)) %>% 
#                           select(country) %>%  
#                           unique %>% 
#                           pull

countries <- countries[!is.na(countries$continent) ,]

# Create new cases new deaths variables
countries <-countries %>% 
              arrange(country, date) %>% 
              group_by(country) %>% 
              mutate( new_cases= cases - lag(cases),
                      new_deaths = deaths - lag(deaths))

countries <- countries  %>% 
                  mutate(new_cases = if_else(is.na(new_cases), cases, new_cases),
                        new_deaths = if_else(is.na(new_deaths), deaths, new_deaths),
                        weekdays = weekdays(date),
                        weekend = ifelse(weekdays %in% c('Saturday', 'Sunday'), 'Weekend', "Weekday"),
                        weekend = as.factor(weekend)) %>% 
                  rename(code = code_3)

# Clean Environment
rm(full_long, state_data, cases_long, confirmed_cases, continents_clean, deaths, deaths_long, full_long_clean)


#### Local ####

url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'

county_data <- read_csv(url)


#NOTE: Not sure what the deal is with "Unknown" counties
county_data <- county_data %>% 
                    filter(county != 'Unknown') %>% 
                    arrange(state, county, date) %>% 
                    group_by(state, county) %>% 
                    mutate(new_cases= cases - lag(cases),
                           new_deaths = deaths - lag(deaths)
)


#Recode NA from first date as equal to cumulative variable
counties <- county_data %>% 
                  mutate(new_cases = if_else(is.na(new_cases), cases, new_cases),
                         new_deaths = if_else(is.na(new_deaths), deaths, new_deaths),
                         weekdays = weekdays(date),
                         weekend = ifelse(weekdays %in% c('Saturday', 'Sunday'), 'Weekend', "Weekday"),
                         weekend = as.factor(weekend)
 )

rm(county_data)
   
#### Themes ####

main_theme <- theme(legend.title = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank()) 
