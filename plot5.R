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


# Previous search on data lead me to determine I can find "Motor" data at Shot.Name, SCC.Level.Three
# and SCC.

# Find "Motor" values and aggregate in just one Vector.

motor_sn <- grepl("[Mm]otor", SCC$Short.Name)
motor_level3 <- grepl("[Mm]otor", SCC$SCC.Level.Three)
motor_level4 <- grepl("[Mm]otor", SCC$SCC.Level.Four)
motor_vehicles <- motor_sn | motor_level3 | motor_level4


# Filtering Motor Vehicles data

mv_SCC <- SCC[motor_vehicles,] %>%
        transform( SCC = as.character(SCC))

NEI_MV <- NEI$SCC %in% mv_SCC$SCC

mv_data <- NEI[NEI_MV,]

# Filter the emissions of Motor at Baltimore

mv_balt <- filter(mv_data, mv_data$fips == "24510")


# Plot the data
par(mfrow = c(1,1))
boxplot(Emissions ~ year, mv_balt, xlab = "Year" , ylab = "Emissions"
        , outline = FALSE , main = "Motor Vehicles Emissions in Baltimore")
dev.copy(png,"plot5.png")
dev.off()
