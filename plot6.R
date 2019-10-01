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

# Filter the emissions of Motor at Baltimore and Los Angeles

mv_bal_los <- filter(mv_data, mv_data$fips =="24510"| mv_data$fips == "06037")

labels <- c("24510" = "Baltimore","06037" = "Los Angeles")

graf_mv <- (ggplot(mv_bal_los, aes(year,Emissions)) 
            + geom_boxplot(outlier.shape = NA) 
            + coord_cartesian(ylim = c(0,20))
            + facet_grid(rows = vars(fips), labeller = labeller(fips = labels))
            #+ theme(plot.title = element_text(hjust = 0.5, size = 14)
            + ggtitle("Emissions for Motor Vehicles"))
graf_mv
dev.copy(png,"plot6.png")
dev.off()