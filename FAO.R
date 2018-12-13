#Cleaning the database (ok)
summary(FAO) #We have missing values
#We will see who are the best producers in food and feed through the years
DataSunRow <- mutate(FAO, Total=apply(FAO[11:63], 1, sum, na.rm = T))
DataSunRow <- DataSunRow %>% 
  group_by(`Area Abbreviation`,Element) %>%
  summarise(Tot_Production = sum(Total))
DataSunRow = DataSunRow[order(-DataSunRow$Tot_Production),c(1,2,3)]
DataSunRowFood <- subset(DataSunRow, Element == "Food")
DataSunRowFeed <- subset(DataSunRow, Element == "Feed")
#Barplot of Food Prod
FoodProd=DataSunRowFood %>% 
  select(`Area Abbreviation`, Element, Tot_Production) %>%
  arrange(Element, -Tot_Production) 
tOPfOOD = FoodProd[1:10,] 
ggplot(data=tOPfOOD, aes(x= reorder(tOPfOOD$`Area Abbreviation`,-tOPfOOD$Tot_Production), y=tOPfOOD$Tot_Production)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

#Barplot of Feed Prod
FeedProd=DataSunRowFeed %>% 
  select(`Area Abbreviation`, Element, Tot_Production) %>%
  arrange(Element, -Tot_Production) 
tOPfeeD = FeedProd[1:10,] 
ggplot(data=tOPfeeD, aes(x= reorder(tOPfeeD$`Area Abbreviation`,-tOPfeeD$Tot_Production), y=tOPfeeD$Tot_Production)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

#We will see which products are the most producted in food and feed




#We will make an interactive plot (shiny) with food and feed for each country

#We will make a map with leaflet