# CA 2 Section-1
# Download the Ni Postcode dataset and type
#getwd() which will result the current working directory and then move the NIPostcode.csv into working directory
# Loading the dataset
ni_postcode_data <- read.csv("NIPostcodes.csv", header = FALSE)

#A) No.Of Rows, strcuture and First 10 rows of NiPostcode dataset.
#To Display No.of Rows
nrow(ni_postcode_data)
#structure of data frame
str(ni_postcode_data)
#first 10 rows of Ni Postcode dataset
head(ni_postcode_data,10)

#B) Add suitable title for each attribute of the data.
#Display the column names of data frame
colnames(ni_postcode_data)
# loading new column names in col_names1
col_names1 <- c("Organisation Name", "Sub-building Name", "Building Name", "Number", 
                "Primary Thorfare", "Alt Thorfare", "Secondary Thorfare", "Locality", 
                "Townland", "Town", "County", "Postcode", "x-coordinates", "y-coordinates", "Primary Key")
# Assigning new Column names
colnames(ni_postcode_data) <- col_names1
# Chechking the column names of data frame
colnames(ni_postcode_data)
# structure of data frame
str(ni_postcode_data)

# To display the rows with all null data
data.frame(sapply(ni_postcode_data, function(x)sum(length(which(is.na(x))))))    
nrow(ni_postcode_data[!complete.cases(ni_postcode_data),])

#c) Replace all Missing Entities with Na
#Intially Assign Na To all empty feilds
ni_postcode_data[ni_postcode_data == ""] <- NA
# Displaying number of rows of data which are incomplete.
nrow(ni_postcode_data[!complete.cases(ni_postcode_data),])
# Displaying no of empty rows across each column
data.frame(sapply(ni_postcode_data, function(x)sum(length(which(is.na(x))))))    
# Plotting missing values
library(VIM)
missing_values <- aggr(ni_postcode_data, prop= FALSE, numbers = TRUE)
summary(missing_values)

#d) Showing total number of missing rows.
data.frame(sapply(ni_postcode_data, function(x)sum(length(which(is.na(x))))))

#e)Moving Primary key to Start of dataset
ni_postcode_data <- ni_postcode_data[,c(15,1:14)]
head(ni_postcode_data)

#f)New dataset whose Locality, townland and town is LIMAVADY
limavady_data<- subset(ni_postcode_data, Locality =="LIMAVADY" | Townland =="LIMAVADY" | Town == "LIMAVADY")
View(limavady_data)
#number of rows
nrow(limavady_data)

#g)saving dataframe Limavady_data into dataset
write.csv(limavady_data, "Limavady.csv")
write.csv(ni_postcode_data, "CleanNIPostCodeData.csv")
