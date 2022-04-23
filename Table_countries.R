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

#Sort descending and show only top 10

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

Amount_Table <-formattable(Amount, align =c("c","c","c","c","c", "c", "c", "c", "c"), 
                           list(`Indicator Name` = formatter(
                             "span", style = ~ style(color = "grey",font.weight = "bold")),
                             `Top 15` = color_tile("grey80","grey94")))

###Now the second graph###
#Redo everything up until the Calculation of percentages 
#(everything above the ######### line)

#summarize
Amount = rename(Amount, Countries = `Together$Countries`)

Amount = rename("Kenya" =  "Africa", Amount$Countries)
Amount = rename(Amount, "Zambia" =  "Africa")
Amount = rename(Amount, "Uganda" =  "Africa")
Amount = rename(Amount, "Uganda" =  "Africa")
Amount = rename(Amount, "Ethiopia" =  "Africa")
Amount = rename(Amount, "Ghana" =  "Africa")
Amount = rename(Amount, "Tanzania" =  "Africa")
Amount = rename(Amount, "Nigeria" =  "Africa")
Amount = rename(Amount, "South Africa" =  "Africa")

## Replace substring of the column in R dataframe

Amount1 = gsub('Kenya','Africa',Amount$Countries)
Amount2 = gsub('Zambia', 'Africa', Amount1)
Amount3 = gsub('Uganda', 'Africa', Amount2)
Amount4 = gsub('Ethiopia', 'Africa', Amount3)
Amount5 = gsub('Ghana', 'Africa', Amount4)
Amount6 = gsub('Tanzania', 'Africa', Amount5)
Amount7 = gsub('Migeria', 'Africa', Amount6)
Amount8 = gsub('South Africa', 'Africa', Amount7)
Amount9 = gsub('Sub-Saharan Africa', 'Africa', Amount8)
Amount9 = gsub('Mozambique', 'Africa', Amount8)
