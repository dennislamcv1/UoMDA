library("tidyverse")
library("janitor")
library("foreign")
library("cowplot")

# Data cleaning straight from the lectures:
data <- read.xport("LLCP2020.XPT", fill = NA) |> 
  clean_names() |>
  filter(between(x_race,1,8)) |>
  filter(between(income2,1,8)) |>
  filter(between(wtkg3,2300,29500)) |>
  filter(between(htm4,91,244)) |>
  filter(between(x_sex,1,2)) |>
  filter(between(x_ageg5yr,1,13))

x_race_levels=c("White", "Black", "Amer. Ind. Alaskan Nat.", "Asian", "Native Haw. or Pac. Isl.", "Other", "Multiracial", "Hispanic")
income2_levels=c("< $10,000", "< $15,000", "< 20,000", "< 25,000", "< 35,000", "< 50,000", "< 75,000", ">= $75,000")
x_sex_levels=c("Male","Female")
x_ageg5yr_levels=c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")

# Now some data manipulation
data <- data |> mutate(
  x_sex=as_factor(x_sex), 
  x_race=as_factor(x_race),
  x_ageg5yr=as_factor(x_ageg5yr))
#  income2=as_factor(income2))

# And now overwrite the coded values with useful strings
levels(data$x_race) <- x_race_levels
levels(data$x_sex) <- x_sex_levels
#levels(data$income2) <- income2_levels
levels(data$x_ageg5yr) <- x_ageg5yr_levels

plots<-NA
# Iterate over race
for (r in levels(data$x_race)) {
  newdata <- data |> filter(x_race==r)
  plot1 <- newdata |> 
    ggplot(aes(x=income2))+ 
    geom_histogram() +
    labs(x="Income code",
         y="# Obs.")
  if (is.na(plots)){
    plots<-vector(plot1)
  }
  #plots<-append(plots, c(plot1))
}
plot_grid(plotlist=plots)
