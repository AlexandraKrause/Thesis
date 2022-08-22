### Method extraction for all topics ###

library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(formattable)
library(tidyr)
library(remotes)
#remotes::install_github('JohnCoene/echarts4r.maps')
library(echarts4r)
library(echarts4r.maps)

#Use the excel sheet
Thesis <- read_excel("./methods-excel.xlsx", sheet = 1)

Together <- Thesis %>% 
  gather(key = "Country_Number", value = "Countries", 
         c(Country_1, Country_2, Country_3, Country_4, Country_5))

#So, the first step is to differentiate between NAs
Together <- Together %>% 
  mutate_all(~replace(., . == "NA", NA))

#Replace general (No researched country, but rather observation of topic)
# Leave in Many (Studies observing many differnt countries)

#Together <- Together %>% 
#  mutate_all(~replace(., . == "General", NA))

#Show the Numbers per topic
Amount <- Together %>%
  count(Together$Countries)

#Exclude NAs
Amount <- na.omit(Amount)

#Calculation of the percentages of the different research countries
Amount <- Amount %>%
  mutate(Percentage = round((n / sum(n) * 100), 0))

################################################################################

#Sort descending and show only top 15

  Amount <- Amount %>% 
    arrange(desc(n))
  Amount = Amount[1:15,]
  Amount$Top15 <- seq.int(nrow(Amount)) 
  
  #Relocate table columns
  Amount <- Amount %>% 
    relocate(Top15, .before = `Together$Countries`)
  
  
  #change names of columns
  Amount = rename(Amount, Countries = `Together$Countries`)
  Amount = rename(Amount, "Absolute number" = n)
  Amount = rename(Amount, "Top 15" = Top15)
  Amount = rename(Amount, "Percentage [%]" = Percentage)

Amount_Table <-formattable(Amount, align =c("c","c","c","c","c", "c", "c", "c",
                                            "c"), 
                           list(`Indicator Name` = formatter(
                             "span", style = ~ style(color = "grey",
                                                     font.weight = "bold")),
                             `Top 15` = color_tile("grey80","grey94")))

#Show Table
Amount_Table

#################
###map all the countries of case studies where y6 counties were included
#(else = "many" in my excel sheet)
echarts4r.maps::em_bank()

df <- data.frame(
  region = c("Uganda","Kenya","Ghana","Zambia", "Ethiopia", 
             "United Republic of Tanzania", "Nigeria", "South_Africa",
  "Mozambique", "Guinea", "Burkina Faso", "Malawi","Indonesia", "Nepal",
  "India", "Bangladesh", "Vietnam","China",
  "Myanmar", "Philippines", "Thailand", "Cambodia", "Pakistan", "Peru",
  "Nicaragua", "Zimbabwe"), 
  value = c(1)
)

df |> 
  e_charts(region) |>
  em_map("World") |> 
  e_map(value, map = "World") |> 
  e_visual_map(value) |> 
  e_theme("infographic")

