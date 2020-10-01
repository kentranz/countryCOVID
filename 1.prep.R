
library(tidyverse)


rm(list = ls())



countyNYT <- read.csv("Documents/PhD/senMovement/countyCOVID/raw/us-counties.csv")

countyNYT %<>% 
  mutate(date = as.Date(date)) %>%
  arrange(county, date) %>%
  
  group_by(county) %>%
  mutate(newCases = 
           case_when(
             row_number() == 1 ~ cases # if first day, then cumulative cases == daily cases
             , TRUE ~ cases - lag(cases)
           )
  ) %>%
  
  mutate(casesTminus1 = lag(newCases)
         , casesTminus2 = lag(casesTminus1)
         ) %>%

  replace_na(list(newCases = 0
                , casesTminus1 = 0
                , casesTminus2 = 0)) 


write.csv(countyNYT
  , file = 'Documents/PhD/senMovement/countyCOVID/cleaned/countyDailyCases.csv')

