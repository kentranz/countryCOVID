
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
                , casesTminus2 = 0)) %>%


  # ADD HOLIDAY
  mutate(anomalousWeekend = 
         case_when(
           date %in% as.Date(c('2020-03-12', '2020-03-13', '2020-03-14', '2020-03-15'
           )) ~ 1
           , TRUE ~ 0
         )
       
       , longWeekend = 
         case_when(
          date %in% as.Date(c(
             '2020-02-15', '2020-02-16', '2020-02-17'
             , '2020-04-10', '2020-04-11', '2020-04-12', '2020-04-13'
             , '2020-05-23', '2020-05-24', '2020-05-25'
             , '2020-07-03', '2020-07-04', '2020-07-05'
             , '2020-09-04', '2020-09-05', '2020-09-06', '2020-09-07'
           ))
           ~ 1
          , TRUE ~ 0
         )
      
       , weekend = case_when(
         lubridate::wday(date, label = TRUE) %in% c('Sat', 'Sun') ~ 1
         , TRUE ~ 0
       )
)



write.csv(countyNYT
  , file = 'Documents/PhD/senMovement/countyCOVID/cleaned/countyDailyCases.csv')

