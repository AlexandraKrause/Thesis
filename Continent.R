### Continent Coding ###

library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(gdata)
library(tidyfst)

#Use the excel sheet
Thesis <- read_excel("./methods-excel.xlsx", sheet = 1)

#Put the country columns together
Amountnew <- Thesis %>% 
  gather(key = "Country_Number", value = "Countries", 
         c(Country_1, Country_2, Country_3, Country_4, Country_5))


#Now summarize all countries on the African continent
str(Amountnew)

african_countries <- c("Uganda|Kenya|Ghana|Zambia|Ethiopia|Tanzania|Nigeria|South Africa|Mozambique|Guinea|Burkina Faso|Malawi")

Amountnew <- Amountnew %>%
  mutate(Countries = str_replace_all(string = Countries,
                                     pattern = african_countries,
                                     replacement = "Africa"))


#Now summarize all countries on the Asian continent
str(Amountnew)

asian_countries <- c("Indonesia|Nepal|India|Bangladesh|Vietnam|China|Myanmar|Philipines|Thailand|Cambodia|Pakistan")

Amountnew <- Amountnew %>%
  mutate(Countries = str_replace_all(string = Countries,
                                     pattern = asian_countries,
                                     replacement = "Asia"))

# Peu belongs to South America and Nicaragua to North America

str(Amountnew)

american_countries <- c("Peru|Nicaragua")

Amountnew <- Amountnew %>%
  mutate(Countries = str_replace_all(string = Countries,
                                     pattern = american_countries,
                                     replacement = "America"))

Amountnew <-replace_dt(Amountnew,from = "Many", to = "Different research areas")


#So, the first step is to differentiate between NAs
Amountnew <- Amountnew %>% 
  mutate_all(~replace(., . == "NA", NA))


#exclude the two comparative, general gender science studies that do not belong 
#in the model. Following countries:

#Amountnew <- Amountnew %>% 
#  mutate_all(~replace(., . == "Canada", NA))

#Amountnew <- Amountnew %>% 
#  mutate_all(~replace(., . == "New England", NA))

#Amountnew <- Amountnew %>% 
#  mutate_all(~replace(., . == "UK", NA))


#or show them as the category "other"
#some countries are also set as NA in excel. comments then show the 
#non-developing country name.

other_countries <- c("UK|New England|Canada")

Amountnew <- Amountnew %>%
  mutate(Countries = str_replace_all(string = Countries,
                                     pattern = other_countries,
                                     replacement = "Other"))


#Replace general (No researched country, but rather observation of topic)
#Leave in Many (Studies observing many different countries)

Amountnew <- Amountnew %>% 
  mutate_all(~replace(., . == "General", NA))

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

#Sort descending and show only top 4

Amountnew <- Amountnew %>% 
  arrange(desc(n))
Amountnew = Amountnew[1:4,]
Amountnew$Top4 <- seq.int(nrow(Amountnew)) 

#Relocate table columns
Amountnew <- Amountnew %>% 
  relocate(Top4, .before = `Amountnew$Countries`)


#Change names of columns
Amountnew = rename(Amountnew, "Continents" = `Amountnew$Countries`)
Amountnew = rename(Amountnew, "Absolute number" = n)
Amountnew = rename(Amountnew, "Top 4" = Top4)
Amountnew = rename(Amountnew, "Percentage [%]" = Percentage)

### Make a table ###

Amount_Table <-formattable(Amountnew, align =c("c","l","c","c","c", "c",
                                               "c", "c", "c"), 
                           list(`Indicator Name` = formatter(
                             "span", style = ~ style(color = "grey",
                                                     font.weight = "bold")),
                             `Top 15` = color_tile("grey80","grey94")))

#Show result of table
Amount_Table 




### Pie chart with tidyverse ###

label <- paste(Amountnew$`Percentage [%]`, "%")

cols <- c("Africa" = "thistle3", "Asia" = "mistyrose2", "America" = "lavender",
          "Different research areas" = "wheat1", "Other" = "mistyrose1")
Amount_Pie <- Amountnew %>% 
  ggplot(aes(x = "", y =Amountnew$`Percentage [%]`, fill = Continents)) +
  geom_col() +
  geom_text(aes(x = 1.6, label = label),color = c("gray10","gray10", "gray10",
                                                  "gray10"),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE) +
  guides(fill = guide_legend(title = "Continents")) +
  coord_polar(theta = "y")+
  theme_void()+
  scale_fill_manual(values = cols)
#other nice colors: cornsilk1 und mistyrose1
#Show the plot:
Amount_Pie

#ggsave(paste0("."),
#       width = 25,
#       height = 14,
#       units = "cm",
#       dpi = 300)

