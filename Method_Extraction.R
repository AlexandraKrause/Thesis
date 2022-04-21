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

################################################################################

#Within the excel sheets, NA values represent sources that do not use methods
#(newspaper articles, policy briefs,...). Here I want to exclude NA in R,
#but still show it in excel

#So, the first step is to differentiate between NAs
Thesis <- Thesis %>% 
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

#Pie chart without tidyverse. This also shows how many colors are needed
Method_Extraction <- pie(n_method$n, labels = n_method$Method, main = "n Method", 
                         col = c("purple", "violetred1", "green3",
                                "cornsilk"), clockwise = TRUE)


#to get a % sign next to the values:
label <- paste(n_method$perc, "%")

#Pie chart with tidyverse
Method_Extraction <- n_method %>% 
  ggplot(aes(x = "", y =perc, fill = Method)) +
  geom_col() +
  geom_label(aes(label = label), color = c("white","white", "white", "white"), 
         position = position_stack(vjust = 0.5),
           show.legend = FALSE) +
  guides(fill = guide_legend(title = " Methods used")) +
  coord_polar(theta = "y")+
  theme_void() +
  scale_fill_manual(values = met.brewer("Cassatt2")[1:4])

#Show the plot:
Method_Extraction

################################################################################
#Now do plot for the main topics:

#Summary of main topics
Thesis$Main_topic_1

#Sort by Main topics (Main_topic_1) to make a new pie chart
Nutrition <- Thesis %>% 
  filter(Main_topic_1 == "Nutrition")

#or use gather function somehow??? to group two main topics together



#count "Nutrition" values
count(Nutrition)


####and again:

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

#Pie chart with tidyverse
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
