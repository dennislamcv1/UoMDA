

library(tidyverse) #Data Manipulation
library(janitor)   #Data cleaning
library(lubridate) #Managing dates (functions: year(), mon(), day())
#library(ggsci)    #Color pallet (this is not necessary at all) 
#I'm using the function scale_color_lancet. But students can use their personalize
#color pallet or the built in ggplot R pallet. 

data <- read.csv("NYC hourly traffic.csv")


data <- data |>
  clean_names() |>
  mutate(date = as.Date(date , format = "%m/%d/%Y"),
         year = year(date),
         mon = month(date),
         day = day(date))


df_daily <- data |>
  filter(mon == 3) |>
  group_by(year,day) |>
  summarise(Daily_Avg = mean(x_vehicles_e_z_pass+x_vehicles_v_toll))

t.test(df_daily[df_daily$year == 2018,]$Daily_Avg,
       df_daily[df_daily$year == 2020,]$Daily_Avg)  
