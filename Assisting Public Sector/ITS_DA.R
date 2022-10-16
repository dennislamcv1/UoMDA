############ ITS Regression ###########

#In this lecture, we will reproduce the Interrupted Time Series tutorial 
#results from the London School of Hygiene and Tropical Medicine. 
#We will also discuss the typical data analysis process and which parts you 
#would jump into as a policy analyst.  

#As you can see, I am using an R script because it is what I usually do as a 
#data analyst and statistician.

#I am using a library called 'pacman' that loads, which means installs and calls 
#the package into the script, using the p_load function. I like it better than
#writing 'library(package)" because I feel it saves time. I don't necessary used
# all of these packages at once, but they are already loaded in case I need them

#Since I am working with an R Script, instead of an R markdown, to run code, 
#I hit ctrl + enter.

library(pacman)

p_load(
  tidyverse,lubridate,janitor,stringr,gtsummary
)

#We can see that it ran without any issues because, in the console, 
#there are no errors or warnings. After loading all the packages, 
#I read the data set and conducted an exploratory analysis. 

data <- read.csv("ITS_DA/sicily.csv")

#As Dr. Lantz said in a previous lecture the data set contains information about 
#the number of acute coronary episodes in Sicily, Italy per month, before and after
#a population level health intervention. 
#Our goal is to evaluate the effectiveness of such intervention.


#One of the reasons that I enjoy the most while working with R is the 
#facility to visualize data files in the Environment; it only takes a click.
#For example, I can click into data, in the Environment panel and we can observe
#the data. We can see that the variables available are: 

#a. year
#b. month
#c. time = elapsed time since the start of the study
#d. aces = count of acute coronary episodes in Sicily per month (the outcome)
#e. smokban = smoking ban (the intervention) coded 0 before intervention, 1 after
#f. pop = the population of Sicily (in 10000s)
#9. stdpop =  age standardized population


#To achieve our goal, which is to determine the effectiveness of the intervention,
#we first create a variable for the standardized rate of acute coronary episodes
#per 100,0000 people

#I'm using mutate to create a new variable called stdRate that is the ratio
# of two variables in the data set, multiplied by 100.000.

data <- data |>
  mutate(stdRate = (aces/stdpop)*100000)

#We will first create some visualization, then some statistics and then we will
#run a model and assess the results. 

#Since we have multiple observations over time, we can create a scatter plot 
#of the standardized rate of acute coronary episodes over time and assess whether 
#there is a difference in the tendency before and after the intervention.

#I start by piping the data set and then creating a ggplot object, mapping in the 
#x-axis the date and in the y-axis the standardized rate. I'm also using geom_point() 
#because it is a scatter plot. I use theme_bw() because it creates outstanding aesthetics
#that are easy on the eyes. However, I'm using theme() to overwrite some aspects of 
#the title. I'm using labs() to modify the title and the axis labels. Additionally, 
#to create a linear tendency line, I'm using geom_smooth. Finally, I made a light-grey 
#rectangle to indicate the post-intervention period using geom_rect().

data |> 
  ggplot(aes(x = time, y = stdRate)) +
  geom_point() +
  theme_bw() +
  labs(
    title = "Acute Coronary Episodes Standardized Rate per 100.000 people",
    x = "Time",
    y = "Standardized Rate per 100.000",
  ) +
  theme(legend.position = "right",
        plot.title=element_text(size=15, 
                                face="bold", 
                                color="black",
                                hjust= 0.5,
                                lineheight=1.2),
        axis.title.y=element_text(size=8,
                                  vjust = 3.5),  
        axis.text.y=element_text(size=8)) +
  scale_x_discrete(limits = paste0(month.abb[data$month], ", ",data$year), 
                   breaks = paste0("Dec", ", ",unique(data$year))
  ) +
  geom_smooth(method='lm', 
              formula= y~x, 
              se = F,   #They can use F instead of FALSE
              size = 1.2,
              colour="lightblue")+
  geom_rect(
    xmin = 36,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = "lightgrey",
    alpha = 0.005)

#For me, creating a plot is an iterative process. I start with the basics, 
#piping the data set and creating the mapping, and work from there. I build the plot
#layer by layer. Every time I add or change something, I run the code, see the plot, 
#and modify it until I am satisfied with the result. 

#The blue line represents the predicted trend based on the unadjusted 
#linear regression model. The white background represents the pre-intervention 
#period; and the grey background the post-intervention period.The plot shows 
#that most points are located below the trend line rather than distributed 
#randomly across the line. The latter would imply a significant decrease 
#in acute coronary episodes after the intervention. 

#From this point on, the analysis is in charge of the statisticians. 
#Given the study design, the available data, and the hypothesis, 
#the statisticians will perform some statistical methods to test the 
#validity of the hypothesis. 

#Before digging into a statistical method appropriate for this particular study design, 
#we will test if the mean of acute coronary episodes before and after the 
#intervention is significantly different. 


#I use tbl_summary from the package gtsummary to customize a summary table of 
#descriptive statistics. I find it especially useful when creating Table 1, 
#which is usually descriptive statistics of the study population while writing a paper. 
#First, I pipe the data and select the variables I want to display on the table; 
#then, I used the tbl_summary function to create statistics by the intervention 
#variable. You can use ?tbl_summary to see an example and all of the features that 
#can be modified. Finally, I include add_p() to compute the hypothesis test. 

data |>
  select(-year,-month, -time, -pop, -stdpop) |>
  mutate(smokban = ifelse(smokban == 0, "Pre-intervention","Post-intervantion"))|>
  tbl_summary(by = smokban,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              digits = all_continuous() ~ 2,
              label = list(aces ~ "Acute Coronary Episodes",
                           stdRate ~ "ACE Standarized Rate")) |>
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) |>
  add_overall() |>
  modify_header(label ~ "**Variable**")|>
  bold_labels()


#For each of variable, the table shows the overall mean, the mean 
#for the pre and post-intervention period, and the p-value 
#testing the difference.  

#Using a 5% significance level, we conclude that the number of acute 
#coronary episodes significantly differs between the pre and 
#post-intervention period (p-value less than 5%). 
#However, the standardized rate is not significantly different 
#when we account for the population size. 

#Because the intervention has a clear cutoff point and the outcome is short term
#we will performed an Interrupted Time Series Analysis, using a Poisson regression.
#The model uses the following structure:

# Y_t = beta_0 + beta_1*T + beta_2*X_t + beta_3 T*X_t 

# T: the time elapsed since the start of the study

# X_t⁠: binary variable for pre-post intervention (smkban);

# Y_t: the number of ACE at time t⁠.

#If you want to deep into the details of the modeling process you can refer to the paper.

model <- glm(aces ~ offset(log(stdpop)) + smokban + time, 
             family=poisson, data)
summary(model)

#The model shows that there is strong evidence 
#(p-value of less than 0.01) of a reduction in 
#ACEs following the smoking ban, with an average decrease of 11%.

#We save the model predictions and then plot the same scatter plot 
#as before, but instead of using a linear trend line, we will use 
#the model predictions. 

data$pred <- exp(predict(model, newdata = data))/mean(data$stdpop)*10^5

#As you can see, I'm using mostly the same code as before, the only difference
#is that instead of using geom_smoth() I'm using the model predictions as a secondary
#set of point to display.

data |> 
  ggplot() +
  geom_point(aes(x = time, y = stdRate), 
             shape = 19,
             color = "black") +
  geom_point(aes(x = time, y = pred), 
             shape = 17,
             color = "red") +
  theme_bw() +
  labs(
    title = "Acute Coronary Episodes Standardized Rate per 100.000 people",
    x = "Time",
    y = "Standardized Rate per 100.000",
  ) +
  theme(legend.position = "right",
        plot.title=element_text(size=15, 
                                face="bold", 
                                color="black",
                                hjust= 0.5,
                                lineheight=1.2),
        axis.title.y=element_text(size=8,
                                  vjust = 3.5),  
        axis.text.y=element_text(size=8)) +
  scale_x_discrete(limits = paste0(month.abb[data$month], ", ",data$year) , 
                   breaks = paste0("Dec", ", ",unique(data$year))
  )+
  geom_rect(
    xmin = 36,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = "lightgrey",
    alpha = 0.009) 


#Notice that we can observe 11% reduction in the standardized rate 
#after the intervention. 

#Today, we walked through an example of assessing the effectiveness of population-level 
#interventions. We used some compelling visuals to see the phenomenon and quantify the 
#effectiveness using some statistical models. Sometimes these models are hard to 
#understand; however, that is the beauty of working on an interdisciplinary team. 
#Interacting with statisticians and people from various backgrounds will be essential 
#to your role.








