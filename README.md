# Master thesis 
# Factors influencing rural farm women’s empowerment in agricultural development

-Literature review/ Decision Analysis Impact Pathway statistics-
<br>
Figures describing my literature research/ Decision Analysis Impact Pathway for my MSc. Thesis, showing distributions, limitations, and others.

I conducted my Literature research to draw a detailed Decision Analysis Impact Pathway, which I later reduced to model it with code. This research included around 160 scientific sources (papers, books). Among these, around 50 papers were mehodological and therefore not included in the model itself, but in the literature review and the following plots. The visual model was created with 102 papers, one presentation and one policy brief. Some resources were only present in the literature review since these studies reflected generalized findings from European case studies, not including developing countries. Therefore they were not included in the model or the following plots.
Many more sources included R.package descriptions and websites, but these were also not included to conduct the figures or the model. Overall, there are about 200 sources.

I extracted 5 case study countries per study. > 5 case study countries were referred to as "Many" in the country table or "Different research areas" in the "Continent"- table instead of listing all countries sperately. These case studies partly contained very many areas and were therefore referred to in this way (also in the excel sheet). For this reason, I also did not include these in the design of the maps.
I also extracted two main topics per scientific resource.
Find the Find the numbered citations list here for more detailed information: https://github.com/AlexandraKrause/Thesis/blob/main/methods-excel.xlsx
Surces used to gather methodological knowledge within my thesis are marked orange, packages green, workforce literature bright blue and literature only used in the introduction or discussion gray. Internet sources are yellow. Further categories on the left side have more pastell color schemes.
Furthermore, find the "Femiaculture" app here: https://github.com/AlexandraKrause/Femiaculture.

Content list of this repository:
- Graphics: The graphics I conducted through coding and using draw.io
- Topics_pie_chart.R: Pie chart of nine main topics found in the literature
- Continent.R: Code to produce case study countries visuals
- Table_countries.R: Code to produce tables of countries where scientists conducted case studies
- Method_Extraction.R: Pie chart of used methods within the case studies
- Methods-per-topic.R: Pie chart of used methods within the case studies per topic (education, health, ...)
- checkLiteratur.R: Checks the excel file for duplicates
- methods-excel.xls: Excel sheet of the used sources within the thesis

# Complete visual model
-> This is the extensive Decision Analysis Impact Pathway, built by literature review and conference attendance. The numbers represent the sources you can find in the excel sheet. Find the numbered citations list here:
https://github.com/AlexandraKrause/Thesis/blob/main/methods-excel.xlsx
And find HTML and URL overview in the "Graphics" section.

![Extended-Decision-Analysis-Impact-Pathway-Alexandra-Krause drawio(2)](https://user-images.githubusercontent.com/82711784/187901773-d1a3ef67-94b9-4c3e-95fa-fc3f3053e29e.png)


# Methodologies
-> A pie chart shows the used methodologies found during my literature research.
![used methods within stuides](https://user-images.githubusercontent.com/82711784/187078507-9020dcc5-61c9-4de6-922c-9b662edb40fa.png)


# In comparison: Number of cited studies per methodology of Pyburn & van Eerdewijk (2021)
![image](https://user-images.githubusercontent.com/82711784/175346910-5405ef3a-2247-415b-b697-2b3788c41ba2.png)

<br><br><br>
<hr><hr>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>



# Country map
-> This map shows in which countries the observed research took place:
![map-scharf](https://user-images.githubusercontent.com/82711784/185958123-1b1d44d0-40e8-4b15-9714-a0a2a26a4041.png)


# Topics
-> I extracted one to two main topics per scientific resource. I found 8 topic areas and studies containing "many" differnt topic to similar amounts. This distribution shows how often which main topics were present within the literature research. These areas can also be found in my detailed Decision Analysis impact pathway.

![main topics](https://user-images.githubusercontent.com/82711784/187078512-1671037d-a37a-48f0-9b5e-1b6e66ef1171.png)


# Topics: Education
-> To overview methods used per topic for all 8 topics, see "Methods-per-topic.R". Here is an example for the topic "Education":

![education](https://user-images.githubusercontent.com/82711784/185979328-aa422ca2-e281-41d7-9cc3-bccfcf217d77.png)


# Literature
Pyburn, Rhiannon, and Anouka Van Eerdewijk (eds).
Advancing Gender Equality through Agricultural and Environmental Research: Past,
Present, and Future. Washington, DC: International Food Policy Research Institute,
2021.
