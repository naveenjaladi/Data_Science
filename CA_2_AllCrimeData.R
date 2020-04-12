# TO run this code NiPostCode data frame is needed so first run the r code from the link
# LINK: https://github.com/naveenjaladi/Data_Science/blob/master/CA_2_NiPostCodes.R

# CA-2 ALL NI Crime Data
# Selecting the Folder containing Ni Crime Data as the working Directory
setwd("NI Crime Data")

# A. Appending all csv files into one data frame called all_crime_data
#Creating a empty dataframe
all_crime_data <- data.frame()
#listing all files in directory
files_list <- list.files(recursive = TRUE)
# Reading csv files in folder into crime_data and then binding the next file with previous files
for(file_list in files_list){
  crime_data <- read.csv(file_list)
  all_crime_data <- rbind(all_crime_data, crime_data)
}
# Displayin number of rown in data frame
nrow(all_crime_data)
# wiriting data frame as CSV File.
write.csv(all_crime_data, "AllNICrimeData.csv")
str(all_crime_data)

# B. Selecting required columns from the dataframe
AllNICrimeData <- all_crime_data[c(2,5,6,7,10)]
str(AllNICrimeData)

#c) Assigning new short form each crime type in the newly created data frame AllNiCrimeData
# To do so first convert crime type which is in factors type into character type 
AllNICrimeData$Crime.type <- as.character(AllNICrimeData$Crime.type)
# Assigning new short forms for each crime type
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Anti-social behaviour"] <- "ASBO"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Bicycle theft"] <- "BITH"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Burglary"] <- "BURG"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Criminal damage and arson"] <- "CDAR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Drugs"] <- "DRUG"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Other theft"] <- "OTTH"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Public order"] <- "PUBO"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Robbery"] <- "ROBY"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Shoplifting"] <- "SHOP"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Theft from the person"] <- "THPR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Vehicle crime"] <- "VECR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Violence and sexual offences"] <- "VISO"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Other crime"] <- "OTCR"
AllNICrimeData$Crime.type[AllNICrimeData$Crime.type == "Possession of weapons"] <- "POS"
str(AllNICrimeData)
#d) ploting the AllNICrimeData type with its frequency or number of crimes
#barplot(table(AllNICrimeData$Crime.type), xlab = "Crime Type", ylab ="Frequency")

library(ggplot2)
ggplot(AllNICrimeData, aes(x=Crime.type)) + geom_bar(fill = "blue", color ="black")

#e) dealing Missing location and making location more precise by removing On or near in location column
# COnverting column Location factor into character
AllNICrimeData$Location <- as.character(AllNICrimeData$Location)
# Removing On or near value from the columns
library(stringr)
AllNICrimeData$Location <- str_remove_all(AllNICrimeData$Location, "On or near ")
# Assigning an identifier NA for No Location and empty feilds
AllNICrimeData$Location[AllNICrimeData$Location == "No Location" | AllNICrimeData$Location == ""] <- NA
str(AllNICrimeData$Location)
# number of location feilds with NA
sum(is.na(AllNICrimeData$Location))

#f) Random sample, seed and finding town comparing ni_postcode_data and random_crime_sample
# Taking subset from the AllNIcrimedata whose location is not NA or which contains a value
crime_sample <- subset(AllNICrimeData[, ], !is.na(AllNICrimeData$Location))
#setting seed as 100
set.seed(100)
# Selecting 5000 rows from random sample which is subset created in prior step
random_crime_sample <- crime_sample[sample(1:nrow(crime_sample), 5000, replace = FALSE), ]
#function of find_a_town that checks location in random Sample and assign town by comparing locality to 
# primary thorfare in ni_post_code and new column is created with town
# As we have captalised words in primary thorfare and town convert them into lower case and then compare
# To convert them we need to import stringr package 
#install.packages("stringr")
library(stringr)

#converting primary thorfare and location to lower case to eliminate case sensivity miss match.
ni_postcode_data$`Primary Thorfare` <- str_to_lower(ni_postcode_data$`Primary Thorfare`)
random_crime_sample$Location <- str_to_lower(random_crime_sample$Location)
str(ni_postcode_data)
str(random_crime_sample)
# Function to find a town
find_a_town <- function(x,y){
  return(ni_postcode_data$Town[match(x,y)])
}
random_crime_sample$Town <- find_a_town(random_crime_sample$Location,ni_postcode_data$`Primary Thorfare`)

#G) Add_town_data check the town in random_crime_sample and village list town and return the population
#Read data village list Csv into data frame
village_list <- read.csv("VillageList.csv")
str(village_list)
# While comparision to eliminate case sensitivity make city and location to lower cases
village_list$ï..CITY.TOWN.VILLAGE <- str_to_upper(village_list$ï..CITY.TOWN.VILLAGE)
random_crime_sample$Location <- str_to_upper(random_crime_sample$Location)
# London derry is given as derry in village list so update it to londonderry using string replace
village_list$ï..CITY.TOWN.VILLAGE <- str_replace_all(village_list$ï..CITY.TOWN.VILLAGE,"DERRY", "LONDONDERRY")
# Function to add population to random crime sample
add_town_data <- function(x,y){
  return(village_list$POPULATION[match(x,y)])
}
random_crime_sample$Population <- add_town_data(random_crime_sample$Town,village_list$ï..CITY.TOWN.VILLAGE)

#H) To rename city-town-village and save as CSV File
colnames(random_crime_sample)[6] <- c("City-Town-Village")
str(random_crime_sample)
write.csv(random_crime_sample, "random_crime_sample.csv")

#i) Plotting crimes from derry and belfast 
# creating two dataframes with derry and belfast data
random_crime_belfast <- subset(random_crime_sample, random_crime_sample$`City-Town-Village` == "BELFAST")
random_crime_london_derry <- subset(random_crime_sample, random_crime_sample$`City-Town-Village` == "LONDONDERRY")
#plotting
library(dplyr)
library(ggplot2)
# plotting belfast crime data with crime type at x and frequency or count at y
plot1 <- random_crime_belfast %>%
  group_by(Crime.type) %>%
  summarise(freq = n()) %>%
  ggplot(aes(x = Crime.type, y = sort(freq, decreasing = TRUE), fill = freq)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Crime Type",
    y = "Count",
    title = paste(
      "Belfast Crime Data"
    )
  )
# plotting London derry crime data with crime type at x and frequency or count at y
plot2 <- random_crime_london_derry %>%
  group_by(Crime.type) %>%
  summarise(freq = n()) %>%
  ggplot(aes(x = Crime.type, y = sort(freq, decreasing = TRUE), fill = freq)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Crime Type",
    y = "frequency",
    title = paste(
      "Derry crime data"
    )
  )
# Plotting both graphs 
install.packages("gridExtra")
require(gridExtra)
grid.arrange(plot1,plot2,ncol=2)

       