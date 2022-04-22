### Method extraction for the topic Nutrition ###

library(readxl)
library(tidyverse)
#library(dplyr)
#install.packages("scales")
#library(scales)
library(ggplot2)
library(MetBrewer)
library(viridis)

#Use the excel sheet
Thesis <- read_excel("./methods-excel.xlsx", sheet = 1)

### In order to show a plot for the main topics, the topics have to be extracted
### 
#Here: Nutrition

#Summary of main topics
Thesis$Main_topic_1


#Sort by Main topics (Main_topic_1) to make a new pie chart
#Nutrition <- Thesis %>% 
#  filter(Main_topic_1 == "Nutrition")
#Show Summary
#Nutrition
#Count "Nutrition" values
#count(Nutrition)

#Now same for second main topic
#Nutrition2 <- Thesis %>% 
#  filter(Main_topic_2 == "Nutrition")
#Nutrition2

#or use gather function to group two main topics together. I decide for this

Together <- Thesis %>% 
  gather(key = "TopicPart", value = "Topics", c(Main_topic_1, Main_topic_2))

#filter topic Nutrition
Nutrition <- Together %>% 
  filter(Topics == "Nutrition")

count(Nutrition)

### Preparation of the method data ###

#So, the first step is to differentiate between NAs
Thesis <- Nutrition %>% 
  mutate_all(~replace(., . == "NA", NA))

#Summary of the data with NA
n_method <- Nutrition %>% 
  count(Method) 

#Summary of the data without NA
n_method <- na.omit(n_method)

#Calculation of the percentages of the different used methods 
n_method <- n_method %>%
  mutate(perc = round((n / sum(n) * 100), 0))

#Shows summary of the data. No NA included.
n_method$n
n_method$Method

#to get a % sign next to the values:
label <- paste(n_method$perc, "%")


### Pie chart with tidyverse ###
Method_Extraction <- n_method %>% 
  ggplot(aes(x = "", y =perc, fill = Method)) +
  geom_col() +
  geom_label(aes(label = label), color = c("white","white", "white"), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "The used methods for the Main topic Nutrition ")) +
  coord_polar(theta = "y")+
  theme_void() +
  scale_fill_manual(values = met.brewer("Cassatt2")[1:3])

#Show the plot:
Method_Extraction
