# This program read data about pollutant emission in the yeas of 1999, 2002, 2005, and 2008
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

##### answer for question 1 STARTS here #####

SCC <- readRDS("./data/Source_Classification_Code.rds")

sum_emi <- tapply(NEI$Emissions, NEI$year, sum)
val_emi <- c(sum_polutant[[1]],sum_polutant[[2]],sum_polutant[[3]], sum_polutant[[4]])
par(mfrow = c(1,2))
plot(c(1999, 2002, 2005, 2008),val_emi, xlab = "Year", ylab = "Sum of Emission (tons)"
     , main = "Total of emission in USA", pch=19)
boxplot(Emissions ~ year, NEI, xlab = "Year" , ylab = "Emissions"
        , outline = FALSE , main = "Spread of Emissions in USA")


#### Answer for question 1 ENDS here   ######

#Investigations for question1 #

x0 <- NEI$Emissions
str(x0)
mean(is.na(x0)) # Checking for amount of NA values -> Nothing
levels(factor(NEI$Pollutant)) # Checking for levels off pollutat -> only one

boxplot(Emissions ~ year, NEI, outline = FALSE)

plot(c(mean(NEI_Y$`1999`$Emissions),mean(NEI_Y$`2002`$Emissions),mean(NEI_Y$`2005`$Emissions),mean(NEI_Y$`2008`$Emissions)), ylab = "")

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


boxplot(Emissions~year,NEI, outline = FALSE, xlab = "Year", ylab = "Emissions")

general_outliers <- NEI$Emissions > 20000
new_data <- NEI[!general_outliers, c(1:2,4:6)]
new_data$year <- factor(new_data$year)
new_data$type <- factor(new_data$type)
boxplot(Emissions ~ year, new_data, outline = FALSE, xlab = "Year") # Through this graphic it´s not clear wether the Emissions
#fall or not. That is because the mean is far from the edges of data, or in other words.
# Too much data out of 6 standard deviation.
# Tyr to calculate the log 10 of emissions
new_data$logpm2.5 <- log10(new_data$Emissions)
boxplot(logpm2.5 ~ year, new_data, outline = FALSE) # The graphic suggests   trend to fall in emissions, but it´s too
#  spready and for a better look t the mean region will cut the Y axis from 5 to -5
# Change ylim from -4 to 3
boxplot(logpm2.5 ~ year, new_data, ylim = c(-4,3))
# Change ylim from -4 to 1
boxplot(logpm2.5 ~ year, new_data, ylim = c(-4,1))

################################################################
############ Investigations for question 1 ENDs here  ##########



###### Question 2 STARTS Here ################################

par(mfrow = c(1,1))
balt_data <- subset(NEI, NEI$fips == "24510")

sum_balt <- tapply(balt_data$Emissions, balt_data$year, sum)
val_balt <- c(sum_balt[[1]],sum_balt[[2]],sum_balt[[3]], sum_balt[[4]])
par(mfrow = c(1,2))
plot(c(1999, 2002, 2005, 2008),val_emi, xlab = "Year", ylab = "Sum of Emission (tons)"
     , main = "Total of emission in Baltimore", pch=19)
boxplot(Emissions ~ year, NEI, xlab = "Year" , ylab = "Emissions"
        , outline = FALSE , main = "Spread of Emissions in Baltimore")

####### Question 2 ENDS Here ##################################

###### Question 3 STARTS Here #########################33

# Observing how the data spread accros the different types

mean_balt <- tapply(balt_data$Emissions, balt_data$type, sum)
summary_balt <- tapply(balt_data$Emissions, balt_data$type, summary)

grf_total <- (ggplot(balt_data, aes(year,Emissions)) 
              + geom_boxplot(outlier.shape = NA) 
              + coord_cartesian(ylim = c(0,5))
              + facet_grid(rows = vars(type)))

# At a first glance it is possible to see that the data from roads and point have 
# different scales, you can check this out running the graphic as follow.
grf_total <- (ggplot(balt_data, aes(year,Emissions)) 
              + geom_boxplot(outlier.shape = NA) 
              + coord_cartesian(ylim = c(0,5))
              + facet_grid(rows = vars(type)))



# By the way, the plots will be splited into to groups:
# Road and Non Road 
# Point and Non Point  

balt_point <- filter(bal_data, type =="POINT" | type =="NONPOINT")
balt_road  <- filter(bal_data, type =="ON-ROAD" | type =="NON-ROAD")

par(mfrow = c(2,1))
graf_road <- (ggplot(balt_road, aes(year,Emissions)) 
        + geom_boxplot(outlier.shape = NA) 
        + coord_cartesian(ylim = c(0,2))
        + facet_grid(rows = vars(type)))

graf_point <- (ggplot(balt_point, aes(year,Emissions)) 
              + geom_boxplot(outlier.shape = NA) 
              + coord_cartesian(ylim = c(0,50))
              + facet_grid(rows = vars(type)))

grid.arrange(graf_road,graf_point,ncol =1)

################  Question 3 ENDS here    ###################


################ Question 4 STARTS Here   ###################

level_ei <- grepl("[Cc]oal", SCC$EI.Sector)
levelOne <- grepl("[Cc]oal", SCC$SCC.Level.One)
levelTwo <- grepl("[Cc]oal", SCC$SCC.Level.Two)
levelThree <- grepl("[Cc]oal", SCC$SCC.Level.Three)
levelFour <- grepl("[Cc]oal", SCC$SCC.Level.Four)

l_with_coal = level_ei|levelThree|levelFour

data_about_coal <- SCC[l_with_coal,] %>%
  transform( SCC = as.character(SCC))

coal_consumers <- NEI$SCC %in% data_about_coal$SCC

coal_data <- NEI[coal_consumers, ]

sum_emi_coal <- tapply(coal_data$Emissions, coal_data$year, sum)
val_emi_coal <- c(sum_emi_coal[[1]],sum_emi_coal[[2]],sum_emi_coal[[3]], sum_emi_coal[[4]])
par(mfrow = c(1,2))
plot(c(1999, 2002, 2005, 2008),val_emi_coal, xlab = "Year", ylab = "Sum of Emission by Coal (tons)"
     , main = "Total of emission by Coal in USA", pch=19)
boxplot(Emissions ~ year, coal_data, xlab = "Year" , ylab = "Emissions"
        , outline = FALSE , main = "Spread of Emissions in USA")