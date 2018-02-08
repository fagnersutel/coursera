# Read data
setwd("/home/fsutel/Documentos/r/eda/ex1/FNEI_data/")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

total <- with(NEI, tapply(Emissions, year, sum))
total
plot(names(total), total, type = 'l', xlab = 'Year', ylab = 'Emissions (tons)', main = 'US Total Emissions')


# Read data

baltimore <- subset(NEI, fips == '24510')
baltimoreTotal <- with(baltimore, tapply(Emissions, year, sum))

#png('plot2.png')
plot(names(baltimoreTotal), baltimoreTotal, type = 'l', xlab = 'Year', ylab = 'Emissions (tons)', main = 'Baltimore Total Emissions')
#dev.off()

#install.packages('dplyr')
library(dplyr)
library(ggplot2)

# Read data
totalByType <- group_by(NEI, type, year) %>% summarise(emissions = sum(Emissions))

qplot(data = totalByType, year, emissions, facets = . ~ type, geom = c('point', 'line')) +
  labs(x = 'Year', y = 'Emissions (tons)', title = 'US Total Emissions')

ggsave('plot3.png')

ibrary(dplyr)
library(ggplot2)

# Read data

coalCombustionRelatedScc <- subset(SCC, grepl('Combustion', SCC.Level.One, ignore.case = TRUE) & grepl('Coal', SCC.Level.Three, ignore.case = TRUE)) %>% select(SCC)
totalCoalCombustionEmissions <- filter(NEI, SCC %in% coalCombustionRelatedScc$SCC) %>% group_by(year) %>% summarise(emissions = sum(Emissions))

qplot(data = totalCoalCombustionEmissions, year, emissions, geom = c('point', 'line')) +
  labs(x = 'Year', y = 'Emissions (tons)', title = 'US Total Coal Combustion Related Emissions')

ggsave('plot4.png')


motorVehicleScc <- subset(SCC, grepl('Motor Vehicle', SCC.Level.Three, ignore.case = TRUE)) %>% select(SCC)
totalMotorVehicleEmissions <- filter(NEI, SCC %in% motorVehicleScc$SCC) %>% group_by(year) %>% summarise(emissions = sum(Emissions))

qplot(data = totalMotorVehicleEmissions, year, emissions, geom = c('point', 'line')) +
  labs(x = 'Year', y = 'Emissions (tons)', title = 'US Total Motor Vehicle Emissions')

ggsave('plot5.png')



fipsData <- data.frame(fips = c('06037', '24510'), location = c('Los Angeles County', 'Baltimore City'))

motorVehicleScc <- subset(SCC, grepl('Motor Vehicle', SCC.Level.Three, ignore.case = TRUE)) %>% select(SCC)
totalMotorVehicleEmissions <- filter(NEI, fips %in% fipsData$fips & SCC %in% motorVehicleScc$SCC) %>% 
  group_by(fips, year) %>% 
  summarise(emissions = sum(Emissions)) %>% 
  inner_join(fipsData)

ggplot(data = totalMotorVehicleEmissions, aes(year, emissions, group = location)) +
  geom_line(aes(color = location)) + 
  geom_point(aes(color = location)) + 
  labs(x = 'Year', y = 'Emissions (tons)', title = 'Total Motor Vehicle Emissions')
