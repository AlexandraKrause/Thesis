### Method extraction for all topics ###

library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)


#Use the excel sheet
Thesis <- read_excel("./methods-excel.xlsx", sheet = 1)

Together <- Thesis %>% 
  gather(key = "Country_Number", value = "Countries", c(Country_1, Country_2, Country_3, Country_4, Country_5))

#So, the first step is to differentiate between NAs
Together <- Together %>% 
  mutate_all(~replace(., . == "NA", NA))


#exclude the two comparative, general gender science studies that do not belong 
#in the modelfrom 
Together <- Together %>% 
  mutate_all(~replace(., . == "Mexico", NA))

Together <- Together %>% 
  mutate_all(~replace(., . == "Canada", NA))

Together <- Together %>% 
  mutate_all(~replace(., . == "New England", NA))

#Replace general (No researched country, but rather observation of topic)
#Leave in Many (Studies observing many differnt countries)

Together <- Together %>% 
  mutate_all(~replace(., . == "General", NA))

#Now summarize all countries on the african continent...does not work.........

Amountnew <- Together %>% 
  mutate_all(~replace(., . == c('Kenya', "Zambia", "Uganda", 'Ethiopia','Ghana',
                                'Tanzania', 'Nigeria', 'South Africa',
                                'Sub-Saharan Africa','Mozambique',
                                'Burkina Faso','Guinea'), 
                      values = c('Africa', 'Africa', 'Africa', 'Africa', 'Africa',
                                 'Africa', 'Africa', 'Africa', 'Africa', 'Africa', 
                                 'Africa','Africa')))

#show summary
Amountnew$Countries

#Show the Numbers per topic
Amountnew <- Amountnew %>%
  count(Amountnew$Countries)

#Exclude NAs
Amountnew <- na.omit(Amountnew)

#Calculation of the percentages of the different research countries
Amountnew <- Amountnew %>%
  mutate(Percentage = round((n / sum(n) * 100), 0))

#Sort descending and show only top 10

Amountnew <- Amountnew %>% 
  arrange(desc(n))
Amountnew = Amountnew[1:15,]
Amountnew$Top15 <- seq.int(nrow(Amountnew)) 

#Relocate table columns
Amountnew <- Amountnew %>% 
  relocate(Top15, .before = `Together$Countries`)


#change names of columns
Amountnew = rename(Amountnew, Countries = `Amountnew$Countries`)
Amountnew = rename(Amountnew, "Absolute number" = n)
Amountnew = rename(Amountnew, "Top 15" = Top15)
Amountnew = rename(Amountnew, "Percentage [%]" = Percentage)

Amount_Table <-formattable(Amountnew, align =c("c","c","c","c","c", "c", "c", "c", "c"), 
                           list(`Indicator Name` = formatter(
                             "span", style = ~ style(color = "grey",font.weight = "bold")),
                             `Top 15` = color_tile("grey80","grey94")))


