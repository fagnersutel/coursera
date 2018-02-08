
dataDir <- "./"
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS(file.path(dataDir,"summarySCC_PM25.rds"))
SCC <- readRDS(file.path(dataDir,"Source_Classification_Code.rds"))

# 1) Have total emissions from PM2.5 decreased in the United States from 1999 to 
# 2008? Using the base plotting system, make a plot showing the total PM2.5 
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.
byYear <- aggregate(NEI$Emissions,by=list(NEI$year), FUN=sum)

#png(filename='plot1.png')

barplot(byYear$x,xlab = "Year",names=byYear$Group.1,main = "Emmissions by Year", 
        ylab = "Emmissions")

#dev.off()



# 2) Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
# plot answering this question.

isBalt <- NEI$fips == "24510"

baltEmission <- NEI[isBalt,]

byYear <- aggregate(baltEmission$Emissions,by=list(baltEmission$year), FUN=sum)

#png(filename='plot2.png')

barplot(byYear$x,xlab = "Year",names=byYear$Group.1,main = "Emmissions by Year for Baltimore City", 
        ylab = "Emmissions")

#dev.off()




# 3) Of the four types of sources indicated by the type (point, nonpoint, 
# onroad, nonroad) variable, which of these four sources have seen decreases in 
# emissions from 1999–2008 for Baltimore City? Which have seen increases in 
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot 
# answer this question.

isBalt <- NEI$fips == "24510"

#png(filename='plot3.png')

ggplot(NEI[isBalt,],aes(x=factor(year),y=Emissions)) + 
  geom_bar(stat="identity") +
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="Year", y="Total PM2.5 Emission in Tons") + 
  labs(title="PM2.5 Emissions for Baltimore City by Source Type")

#dev.off()


# 4) Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

isCombustion <- grepl("Combustion", SCC$SCC.Level.One)
isCoal <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 

coalCombSCC <- SCC[isCombustion & isCoal, ]
coalCombNEI <- NEI[NEI$SCC %in% as.character(coalCombSCC$SCC),]

byYear <- aggregate(coalCombNEI$Emissions,by=list(coalCombNEI$year), FUN=sum)

#png(filename='plot4.png')

barplot(byYear$x,xlab = "Year",names=byYear$Group.1,main = "Coal Combustion Emmissions in US by Year", 
        ylab = "Emmissions")

#dev.off()


# 5) How have emissions from motor vehicle sources changed from 1999–2008 in 
# Baltimore City?

isBalt <- NEI$fips == "24510"
isVehicle <- grepl("vehicle",SCC$SCC.Level.Two,ignore.case = TRUE)

vehicleSCC <- SCC[isVehicle, ]
vehicleBaltNEI <- NEI[NEI$SCC %in% as.character(vehicleSCC$SCC) & isBalt,]

byYear <- aggregate(vehicleBaltNEI$Emissions,by=list(vehicleBaltNEI$year), FUN=sum)

#png(filename='plot5.png')

barplot(byYear$x,xlab = "Year",names=byYear$Group.1,main = "Baltimore City Motor Vehichle Emissions by Year", 
        ylab = "Emmissions")

#dev.off()



# 6) Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater changes over time in motor 
# vehicle emissions?

isBalt <- NEI$fips == "24510"
isLA <- NEI$fips == "06037"

isVehicle <- grepl("vehicle",SCC$SCC.Level.Two,ignore.case = TRUE)

vehicleBaltNEI <- NEI[NEI$SCC %in% as.character(vehicleSCC$SCC) & isBalt,]
vehicleBaltNEI$city <- rep("Baltimore City",nrow(vehicleBaltNEI))

vehicleLANEI <- NEI[NEI$SCC %in% as.character(vehicleSCC$SCC) & isLA,]
vehicleLANEI$city <- rep("Los Angeles",nrow(vehicleLANEI))

laBaltRows <- rbind(vehicleLANEI,vehicleBaltNEI)

#png(filename='plot6.png')

ggplot(laBaltRows,aes(x=factor(year),y=Emissions)) + 
  geom_bar(stat="identity") +
  facet_grid(scales="free", space="free", .~city) +
  labs(x="Year", y="Total PM2.5 Emission in Tons") + 
  labs(title="PM2.5 Emissions for Baltimore City and Los Angeles")

#dev.off()