library(readxl)
library(tidyverse)
#library(dplyr)
#install.packages("scales")
#library(scales)
library(ggplot2)

Thesis <- read_excel("./methods-excel.xlsx", sheet = 1)
Thesis <- Thesis %>% 
  mutate_all(~replace(., . == "NA", NA))

n_method <- Thesis %>% 
  count(Method) 

n_method <- na.omit(n_method)

n_method <- n_method %>%
  mutate(perc = n / sum(n) * 100)

n_method$n
n_method$Method


Method_Extraction <- pie(n_method$n, labels = n_method$Method, main = "n Method", 
                         col = c("purple", "violetred1", "green3",
                                "cornsilk", "cyan", "white"), clockwise = TRUE)

Method_Extraction2 <- n_method %>% 
  ggplot(aes(x = "", y=perc, fill = Method)) +
  geom_col() +
  geom_text(aes(label = Method,
                position=position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")
  


"Qualitative","Quantitative","Mixed","Methodological", "Other"