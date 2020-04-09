getwd()
mydata <- read.csv("Diabetes-md.csv")

#2
class(mydata)
str(mydata)

#3
mydata[!complete.cases(mydata),]

#Some feilds have missing data and needs to be recoded
#for address
mydata$Address[mydata$Address==""] <- NA
#for type
mydata$Daibetes.type[mydata$Daibetes.type==""] <- NA
#for status
mydata$Status[mydata$Status==""] <- NA

mydata[!complete.cases(mydata),]
str(mydata)
#this shows that it still contains so we update we need to covert to char first and recode to factor

mydata$Daibetes.type <- as.character(mydata$Daibetes.type)

#then convert to factors
mydata$Daibetes.type <- as.factor(mydata$Daibetes.type)
str(mydata)


# For address
mydata$Address <- as.character(mydata$Address)
#mydata$Address <- as.factor(mydata$Address)
str(mydata)

#For Status
mydata$Status <- as.character(mydata$Status)
mydata$Status <- as.factor(mydata$Status)
str(mydata)

# deal with the missing data and chart missing data first.

#analysing data
my_na <- mydata[!complete.cases(mydata),]
nrow(my_na)
# For graphic options
install.packages("mice")
library(mice)
md.pattern(mydata)

#use vim package to show missing values
install.packages("VIM")
library(VIM)

missing_values <- aggr(mydata, prop = FALSE, numbers = TRUE)
summary(missing_values)

#missing address are not important as they donot impact data
#So remove na feilds in type and status feild.

my_na <- mydata[!complete.cases(mydata$Daibetes.type, mydata$Status),]
my_na
nrow(my_na)

#full data with all rows of status and type
full_data <- mydata[complete.cases(mydata$Daibetes.type, mydata$Status),]
nrow(full_data)

#Q4 configure to an unordered factor
# refactor status
mydata$Status <- factor(mydata$Status, order = TRUE, levels = c("Poor", "Improved", "Excellent"))
str(mydata)

#Q5 Changing column names
col_names = c("Patient Name", "NI Address", "Type", "Age", "Health Status")
colnames(mydata) <- col_names
head(mydata, 10)

#Q6 data frames into new data frame 
patient_names <- mydata[1:1]
head(patient_names, 10)
