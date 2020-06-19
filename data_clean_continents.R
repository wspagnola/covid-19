

library(tidyverse)

# Create clean continents dataset

#Continents/Country Dataset
#https://www.kaggle.com/statchaitya/country-to-continent/data#

continents <- read_csv('data/continents_raw.csv')
continents <- continents  %>% select(country, continent, code_3)

continents_clean <- continents %>% 
  mutate(country = 
           case_when(country == 'Korea (Republic of)' ~ 'South Korea',
                     country == 'Russian Federation' ~ "Russia",
                     country =='Viet Nam' ~ 'Vietnam',
                     country ==  'US' ~ 'United States of America',
                     country == 'United Kingdom of Great Britain and Northern Ireland' ~ 'United Kingdom',
                     country == 'Brunei Darussalam' ~ 'Brunei',
                     country ==  "Congo (Democratic Republic of the)" ~ 'Democratic Republic of the Congo',   
                     country == "Syrian Arab Republic" ~ "Syria",
                     country == "Lao People's Democratic Republic" ~ 'Laos', 
                     str_detect(country, "Ivoire") == T ~ "Cote d'Ivoire",
                     
                     TRUE ~ country)) %>% 
  mutate( country = str_remove(country, " \\(.*"),
          country = str_remove(country, "\\,.*"))

write_csv(continents_clean, 'data/continents_clean.csv')
