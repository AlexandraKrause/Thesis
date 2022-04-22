### Code for a pie chart showing which topics occurred how often within the ###
### literature research ###

library(readxl)
library(tidyverse)
library(ggplot2)
#library(MetBrewer)
library(RColorBrewer)

#Use the excel sheet
Thesis <- read_excel("./methods-excel.xlsx", sheet = 1)

### In order to show a plot for the main topics, the topics have to be extracted
### 
#Here: Nutrition

#Summary of main topics
Thesis$Main_topic_1

#Use gather function to group two main topics together.

Together <- Thesis %>% 
  gather(key = "TopicPart", value = "Topics", c(Main_topic_1, Main_topic_2))

#############################################################################
Together$Topics


#So, the first step is to differentiate between NAs
Together <- Together %>% 
  mutate_all(~replace(., . == "NA", NA))


#Show the Numbers per topic
Amount <- Together %>%
  count(Together$Topics)

#Exclude NAs
Amount <- na.omit(Amount)

#Calculation of the percentages of the different used methods 
Amount <- Amount %>%
  mutate(perc = round((n / sum(n) * 100), 0))

#to get a % sign next to the values:
label <- paste(Amount$perc, "%")

#Show summary
Amount$`Together$Topics`

### Pie chart with tidyverse ###
Method_Extraction <- Amount %>% 
  ggplot(aes(x = "", y =perc, fill = `Together$Topics`)) +
  geom_col() +
  geom_text(aes(x = 1.6, label = label),
            position = position_stack(vjust = .5),
            show.legend = FALSE) +
  guides(fill = guide_legend(title = "Main Topics ")) +
  coord_polar(theta = "y")+
  theme_void() +
  scale_fill_brewer(palette = "Pastel1")

#Show the plot:
Method_Extraction