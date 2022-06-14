#Check literature for doubles
library(ggplot2)
library(readxl)

checkLiteratureTemp <- read_excel("methods-excel.xlsx")

checkLiterature <- checkLiteratureTemp[!is.na(checkLiteratureTemp$Authors),]
doubledLiterature <- checkLiterature[duplicated(checkLiterature$Authors),]
