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

# Observing how the data spread accros the different types

balt_data <- subset(NEI, NEI$fips == "24510")
mean_balt <- tapply(balt_data$Emissions, balt_data$type, sum)
summary_balt <- tapply(balt_data$Emissions, balt_data$type, summary)

# Run the graf below for data exploration
# grf_total <- (ggplot(balt_data, aes(year,Emissions)) 
#               + geom_boxplot(outlier.shape = NA) 
#               + coord_cartesian(ylim = c(0,5))
#               + facet_grid(rows = vars(type)))

# At a first glance it is possible to see that the data from roads and point have 
# different scales, you can check this out running the graphic as follow.
# grf_total <- (ggplot(balt_data, aes(year,Emissions)) 
#               + geom_boxplot(outlier.shape = NA) 
#               + coord_cartesian(ylim = c(0,5))
#               + facet_grid(rows = vars(type)))



# By the way, the plots will be splited into to groups:
# Road and Non Road 
# Point and Non Point 
# Investigating data per year and type

with(filter(balt_data, type == "NON-ROAD"), tapply(Emissions, year, summary))
with(filter(balt_data, type == "ON-ROAD"), tapply(Emissions, year, summary))
with(filter(balt_data, type == "NONPOINT"), tapply(Emissions, year, summary))
with(filter(balt_data, type == "POINT"), tapply(Emissions, year, summary))


balt_point <- filter(balt_data, type =="POINT" | type =="NONPOINT")
balt_road  <- filter(balt_data, type =="ON-ROAD" | type =="NON-ROAD")

graf_road <- (ggplot(balt_road, aes(year,Emissions)) 
              + geom_boxplot(outlier.shape = NA) 
              + coord_cartesian(ylim = c(0,2))
              + facet_grid(rows = vars(type)))

graf_point <- (ggplot(balt_point, aes(year,Emissions)) 
               + geom_boxplot(outlier.shape = NA) 
               + coord_cartesian(ylim = c(0,50))
               + facet_grid(rows = vars(type)))


grid.arrange(graf_road,graf_point,ncol =1)
dev.copy(png,"plot3.png")
dev.off()
