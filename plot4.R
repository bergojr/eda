# This program read data about pollutant emission in the years of 1999, 2002, 2005, and 2008


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

# There are more than one colunm related to coal, the idea is to find each  onde of that 
# and aggregate in just one vector

level_ei <- grepl("[Cc]oal", SCC$EI.Sector)
levelOne <- grepl("[Cc]oal", SCC$SCC.Level.One)
levelTwo <- grepl("[Cc]oal", SCC$SCC.Level.Two)
levelThree <- grepl("[Cc]oal", SCC$SCC.Level.Three)
levelFour <- grepl("[Cc]oal", SCC$SCC.Level.Four)

# Aggregated vector
l_with_coal = level_ei|levelThree|levelFour

# Filtering data IN SCC that is related to COAL

data_about_coal <- SCC[l_with_coal,] %>%
        transform( SCC = as.character(SCC))

# Find places where COAL Emission is present

coal_consumers <- NEI$SCC %in% data_about_coal$SCC

coal_data <- NEI[coal_consumers, ]

sum_emi_coal <- tapply(coal_data$Emissions, coal_data$year, sum)
val_emi_coal <- c(sum_emi_coal[[1]],sum_emi_coal[[2]],sum_emi_coal[[3]], sum_emi_coal[[4]])

# Plotting data

par(mfrow = c(1,2))
plot(c(1999, 2002, 2005, 2008),val_emi_coal, xlab = "Year", ylab = "Sum of Emission by Coal (tons)"
     , main = "Total of emission by Coal in USA", pch=19)
boxplot(Emissions ~ year, coal_data, xlab = "Year" , ylab = "Emissions"
        , outline = FALSE , main = "Spread of Emissions in USA")
dev.copy(png,"plot4.png")
dev.off()
