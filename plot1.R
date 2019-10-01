# This program read data about pollutant emission in the years of 1999, 2002, 2005, and 2008
#

library(dplyr)
library(ggplot2)
library(gridExtra)

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

# Read the files 
NEI <- readRDS("./data/summarySCC_PM25.rds") %>%
        transform(year = factor(year)) %>%
        transform(type = factor(type))

SCC <- readRDS("./data/Source_Classification_Code.rds")

# Calculating the total of emission per year
sum_emi <- tapply(NEI$Emissions, NEI$year, sum)
val_emi <- c(sum_emi[[1]],sum_emi[[2]],sum_emi[[3]], sum_emi[[4]])

par(mfrow = c(1,2))
plot(c(1999, 2002, 2005, 2008),val_emi, xlab = "Year", ylab = "Sum of Emission (tons)"
     , main = "Total of emission in USA", pch=19)
boxplot(Emissions ~ year, NEI, xlab = "Year" , ylab = "Emissions"
        , outline = FALSE , main = "Spread of Emissions in USA")
dev.copy(png,"plot1.png")
dev.off()