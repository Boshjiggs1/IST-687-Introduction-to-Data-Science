#IST 687 - Introduction to Data Science - Final Project 
library(readr)
housing <- read_csv("D:/Masters in Data Science/Syracuse University/Classes/IST 687 - M402   Introduction to Data Science/housing.csv")

#-- Column specification -------------------------------------------------------------------------------------------------
cols(
  longitude = col_double(),
  latitude = col_double(),
  housing_median_age = col_double(),
  total_rooms = col_double(),
  total_bedrooms = col_double(),
  population = col_double(),
  households = col_double(),
  median_income = col_double(),
  median_house_value = col_double(),
  ocean_proximity = col_character()
)

View(housing)
dim(housing) # to see how many rows and columns are there in the dataset 
any(is.na(housing)) #  to see if NAs are in the dataset 
colnames(housing)[colSums(is.na(housing))>0] # to see which columns contain NAs 
housing$housing_median_age[is.na(housing$housing_median_age)] <- mean(housing$housing_median_age, na.rm = TRUE) # to change the NAs in column to the mean
housing$total_rooms[is.na(housing$total_rooms)] <- mean(housing$total_rooms, na.rm = TRUE)
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- mean(housing$total_bedrooms, na.rm = TRUE)
housing$population[is.na(housing$population)] <- mean(housing$population, na.rm = TRUE)
housing$households[is.na(housing$households)] <- mean(housing$households, na.rm = TRUE) 
housing$median_income[is.na(housing$median_income)] <- mean(housing$median_income, na.rm = TRUE)

str(housing) # checking the structure of the values in the dataset 

final_project <- data.frame(housing)
View(final_project)
colnames(final_project) <- c("LONGITUDE", "LATITUDE", "HOUSING_MEDIAN_AGE", "TOTAL_ROOMS", "TOTAL_BEDROOMS", "POPULATION", "HOUSEHOLDS", "MEDIAN_INCOME", "MEDIAN_HOUSE_VALUE", "OCEAN_PROXIMITY")
#Changing the column names to upper cases
dim(final_project) # checking to see all the NAs were convereted to means by double checking the number of rows. The result showed 2981 which is correct

medianIncomeHistogram <- hist(housing$median_income, main= "Histogram of Median Income", xlab="Median Income",border="red")
medianIncomeHistogram

install.packages("ggThemeAssist")
install.packages("esquisse")

valueAge <- ggplot(final_project) +
  geom_point(aes(x=final_project$HOUSING_MEDIAN_AGE, y=final_project$MEDIAN_HOUSE_VALUE))

valueAge + theme(panel.grid.major = element_line(linetype = "dotted"),
    panel.grid.minor = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = "yellow2"),
    plot.background = element_rect(fill = "azure1"))

valueAge1 <- ggplot(final_project) +
  geom_boxplot(aes(x=final_project$HOUSING_MEDIAN_AGE, y=final_project$MEDIAN_HOUSE_VALUE))

valueAge1

remotes::install_github("dreamRs/datamods")

library(sf)

plot_usmap(include = .pacific, labels = TRUE)

plot_usmap(include = "CA", labels = TRUE)

           
install.packages("leaflet")


m <- leaflet() %>% setView(lng = -122.23, lat = 37.88, zoom = 12)
m %>% addTiles()

m

b <-leaflet() %>% addTiles() %>%
  addRectangles(
    lng1=-122.23, lat1=37.88,
    lng2=-119.02, lat2=35.34,
    fillColor = "transparent"
  )

b

a <- leaflet(data = final_project[1:3,]) %>% addTiles() %>%
  addCirleMarkers(lng = final_project$LONGITUDE, lat = final_project$LATITUDE, color = )
addPopups(a,lng = final_project$LONGITUDE, lat = final_project$LATITUDE)
a

projection(a,valueAge)

homeAgeRoomTotal <- ggplot(final_project) +
  aes(x = HOUSING_MEDIAN_AGE, y = TOTAL_ROOMS) +
  geom_point(
    shape = "circle cross",
    size = 1.5,
    colour = "#4089D0"
  ) +
  geom_smooth(span = 0.75) +
  labs(title = "Housing Median Age Vs. Total Rooms") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

homeAgeRoomTotal + theme(panel.background = element_rect(fill = "aliceblue",
    linetype = "dotted"), plot.background = element_rect(colour = "aliceblue"))

homeAgeRoomTotal

library(ggplot2)

homeAgeIncome <- ggplot(final_project) +
  aes(x = HOUSING_MEDIAN_AGE, y = MEDIAN_INCOME) +
  geom_point(
    shape = "square open",
    size = 1.5,
    colour = "#16B1C4"
  ) +
  geom_smooth(span = 0.75) +
  labs(title = "Median age of House and Median Income") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  facet_wrap(vars(OCEAN_PROXIMITY))

homeAgeIncome


ageIncomeBoxplot <- ggplot(final_project) +
  aes(x = final_project$HOUSING_MEDIAN_AGE, y = final_project$MEDIAN_INCOME) +
  geom_boxplot()+
  theme_minimal()

ageIncomeBoxplot1 <- ageIncomeBoxplot + theme(plot.title = element_text(face = "bold",
    hjust = 0.5)) +labs(title = "Housing Median Age and Median Income",
    x = "Housing Median age", y = "Median Income")


ageIncomeBoxplot1

library(ggplot2)

proximityMedianAgeBoxplot <- ggplot(final_project) +
 aes(x = OCEAN_PROXIMITY, y = HOUSING_MEDIAN_AGE) +
 geom_boxplot(shape = "circle", 
 fill = "#DDE6F6") +
 labs(x = "Ocean Proximity", y = "Housing Median Age", title = "Ocean Proximity and Median age") +
 theme_gray() +
 theme(plot.title = element_text(face = "bold", hjust = 0.5))


proximityMedianAgeBoxplot


install.packages("tinygeocoder")





homeAgeValue <- ggplot(final_project) +
  aes(x = HOUSING_MEDIAN_AGE, y = MEDIAN_HOUSE_VALUE) +
  geom_point(
    shape = "square filled",
    size = 1.5,
    colour = "#16B1C4"
  ) +
  geom_smooth(span = 0.75) +
  labs(title = "Housing Median age and Median Value") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  facet_wrap(vars(OCEAN_PROXIMITY))

homeAgeValue
