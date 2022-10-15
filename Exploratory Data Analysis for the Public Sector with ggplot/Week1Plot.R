library(tidyverse)
library(janitor)

data<-read_csv("hld") |> clean_names()
data |> 
  filter(region==0, ethnicity==0, age==0, type_lt==4,residence==0) |> 
  filter(country=='MYS') |> 
  ggplot(aes(x=year1,y=e_x)) + 
  geom_line(aes(color=as_factor(sex)))
