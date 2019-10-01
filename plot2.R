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

par(mfrow = c(1,1))
balt_data <- subset(NEI, NEI$fips == "24510")

sum_balt <- tapply(balt_data$Emissions, balt_data$year, sum)
val_balt <- c(sum_balt[[1]],sum_balt[[2]],sum_balt[[3]], sum_balt[[4]])

par(mfrow = c(1,2))
plot(c(1999, 2002, 2005, 2008),val_balt, xlab = "Year", ylab = "Sum of Emission (tons)"
     , main = "Total of emission in Baltimore", pch=19)
boxplot(Emissions ~ year, balt_data, xlab = "Year" , ylab = "Emissions"
        , outline = FALSE , main = "Spread of Emissions in Baltimore")
dev.copy(png,"plot2.png")
dev.off()


