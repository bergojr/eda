# This program read data about pollutant emission in the yeas of 1999, 2002, 2005, and 2008
#

library(dplyr)
setwd("C:/dados/coursera/dscoursera/eda")

fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
fileName <- "FNEI_data.zip"
directory <- "data"

# Download files if not available

if(!file.exists(fileName)){
  download.file(fileURL,fileName) 
}

if(!file.exists(directory)){
  unzip(fileName, files = NULL, exdir="./data")
}

# Read the files necesssary to tidy
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

x0 <- NEI$Emissions
str(x0)
mean(is.na(x0)) # Checking for amount of NA values -> Nothing
levels(factor(NEI$Pollutant)) # Checking for levels off pollutat -> only one

NEI_Y <- split(NEI,NEI$year)

# Checking data by years

# Year: 1999

boxplot(NEI_Y$`1999`$Emissions) # It seems to have an outlier that will be removed
range(NEI_Y$`1999`$Emissions)
outlier_1999 <- NEI_Y$`1999`$Emissions >20000
data_1999 <- NEI_Y$`1999`[!outlier_1999, c(1:2,4:5)]
par(mfrow = c(1,2))
boxplot(NEI_Y$`1999`$Emissions)
boxplot(log10(data_1999$Emissions))

# Year: 2002

boxplot(NEI_Y$`2002`$Emissions) # It seems to have an outlier that will be removed
range(NEI_Y$`2002`$Emissions)
outlier_2002 <- NEI_Y$`2002`$Emissions >20000
data_2002 <- NEI_Y$`2002`[!outlier_2002, c(1:2,4:5)]
par(mfrow = c(1,2))
boxplot(data_2002$Emissions)
boxplot(log10(data_2002$Emissions))

# Year: 2005

boxplot(NEI_Y$`2005`$Emissions) # It seems to have an outlier that will be removed
range(NEI_Y$`2005`$Emissions)
outlier_2005 <- NEI_Y$`2005`$Emissions >20000
data_2005 <- NEI_Y$`2005`[!outlier_2005, c(1:2,4:5)]
par(mfrow = c(1,2))
boxplot(data_2005$Emissions)
boxplot(log10(data_2005$Emissions))


# Year: 2008

boxplot(NEI_Y$`2008`$Emissions) # It seems to have an outlier that will be removed
range(NEI_Y$`2008`$Emissions)
outlier_2008 <- NEI_Y$`2005`$Emissions >20000
data_2008 <- NEI_Y$`2005`[!outlier_2008, c(1:2,4:5)]
par(mfrow = c(1,2))
boxplot(data_2008$Emissions)
boxplot(log10(data_2008$Emissions))

# After the initil exploration of data it was realized the some outliers can be striped from data
# assuming  threshold of 20.000 tons/pm2.5 per year

general_outliers <- NEI$Emissions > 20000
new_data <- NEI[!general_outliers, c(1:2,4:6)]
new_data$year <- factor(new_data$year)
boxplot(Emissions ~ year, new_data) # Through this graphic it´s not clear wether the Emissions
#fall or not. That is because the mean is far from the edges of data, or in other words.
# Too much data out of 6 standard deviation.
# Tyr to calculate the log 10 of emissions
new_data$logpm2.5 <- log10(new_data$Emissions)
boxplot(logpm2.5 ~ year, new_data) # The graphic suggests   trend to fall in emissions, but it´s too
#  spready and for a better look t the mean region will cut the Y axis from 5 to -5
# Change ylim from -4 to 3
boxplot(logpm2.5 ~ year, new_data, ylim = c(-4,3))
# Change ylim from -4 to 1
boxplot(logpm2.5 ~ year, new_data, ylim = c(-4,1))

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")


activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activity_labels) <- c("cod_activity","activity")


# Task 1 - Combining data sets for test and train 
complete_test <- cbind(subject_test,x_test, y_test)
complete_train <- cbind(subject_train,x_train, y_train)
complete_data <- rbind (complete_test, complete_train)


# Task 4 - the reason I´ve done step 4 before steps 2 and 3 is that I would like to name all
# the variables at data set and not only those selected to this lesson.

x_labels <- read.table("./UCI HAR Dataset/features.txt")[,2] 
x_labels <- as.character(x_labels)
x_labels <- gsub("[()]","",x_labels)

names_completedata <- c("subject", x_labels ,"cod_activity")
names(complete_data) <- names_completedata

#Task 2 - Extracts only the measurements on the mean and standard deviation for each measuremen

selected_x <- grep("mean|std", names_completedata)
dataselected <- complete_data[,c(1,selected_x,563)]

# Task 3 - Uses descriptive activity names to name the activities in the data set

data_subject_activity <- merge(dataselected,activity_labels,by.x = "cod_activity",
                    by.y = "cod_activity") %>%
  select(-cod_activity)

# Task 5 - From the data set in step 4, creates a second, independent tidy data set

grouped_data <- tbl_df(data_subject_activity) %>%
  arrange(subject) %>%
  group_by(subject,activity)

summarized_data <- summarize_all(grouped_data, mean) %>%
        arrange(subject,activity)

write.table(summarized_data,"tidysensordata.txt", row.names = FALSE )
