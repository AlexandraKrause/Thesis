### Method extraction for the different topics###
library(readxl)
library(tidyverse)
library(ggplot2)
#library(MetBrewer)

#Use the excel sheet
Thesis <- read_excel("./methods-excel.xlsx", sheet = 1)

### In order to show a plot for the main topics, the topics have to be extracted
### 

#Summary of main topics
Thesis$Main_topic_1
Thesis$Main_topic_2

###############################################################################
#I decided ti use the gather function and have up to two main topics. 
#Else this code would woork:

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
###############################################################################

#Use gather function to group two main topics together

Together <- Thesis %>% 
  gather(key = "TopicPart", value = "Topics", c(Main_topic_1, Main_topic_2))

#Filter the topic
# Insert the names Economy,Education,Health,Nutrition,Resources,Safety,Social
#and Time to see the pie chart for the named topic. 

#Within the pie chart you then have to assemble the number of colors: 
#Not all main topic charts have equal numbers of slides.
#For example the main topic Resources does also contain
#methodological studies. The pie chart of Education topics only has 2 colors.

Filtered <- Together %>% 
  filter(Topics == "Education")

count(Filtered)

### Preparation of the method data ###

#So, the first step is to differentiate between NAs
Thesis <- Filtered %>% 
  mutate_all(~replace(., . == "NA", NA))

#Summary of the data with NA
n_method <- Thesis %>% 
  count(Method) 

#Summary of the data without NA
n_method <- na.omit(n_method)

#Calculation of the percentages of the different used methods 
n_method <- n_method %>%
  mutate(perc = round((n / sum(n) * 100), 0))

#Shows summary of the data. No NA included.
n_method$n
n_method$Method

#To get a percentage (%) sign next to the values:
label <- paste(n_method$perc, "%")


### Pie chart with tidyverse ###
# To adjust to different topics, change the number of "grey10" as the color of 
#labels for each slice. 

cols <- c("Quantitative" = "thistle", "Qualitative" = "pink3",
          "Methodological" = "plum4", "Mixed" = "lightpink2")
Method_Extraction <- n_method %>% 
  ggplot(aes(x = "", y =perc, fill = Method)) +
  geom_col() +
  geom_label(aes(label = label), color = c("gray10","gray10","gray10", "gray10" ), 
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Methods")) +
  coord_polar(theta = "y")+
  theme_void()+
  scale_fill_manual(values = cols)

#Show the plot:
Method_Extraction

#Alternative colors to insert above fo a single graphic:
#scale_fill_manual(values = met.brewer("Cassatt2")[1:3])
