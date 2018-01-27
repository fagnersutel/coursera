setwd("../Exploratory Data Analysis/week4/coursera/eda/ex1/")
#load data
summarySCC_PM25 <- readRDS("FNEI_data/summarySCC_PM25.rds")
#Load code classification
Source_Classification_Code <- readRDS("FNEI_data/Source_Classification_Code.rds")
#Aggregate data by soma of the emmissions/year
totalNEI <- aggregate(Emissions ~ year, summarySCC_PM25, sum)
#define dimensions of plot device
par(mfrow=c(1,2))
#plot line graph
plot(totalNEI$year, ((totalNEI$Emissions)/10^6), type = "o", col = "red", 
     main = expression("Emissoes PM2.5 por Ano nos EUA"), ylab = expression("Emissoes nos EUA de PM2.5 (10^6 Toneladas)"), xlab = "Ano")

#plot barchart
barplot(
    ((totalNEI$Emissions)/10^6),
    col=rgb(0.5,0.1,0.6,0.6),
    totalNEI$year,
    xlab="Ano",
    ylab="Emissoes nos EUA de PM2.5 (10^6 Toneladas)",
    main="Emissoes PM2.5 por Ano nos EUA"
)
#Redefine dimensions of device
par(mfrow=c(1,1))

#Open device png
png("Plot1.png", width=480, height=480)
plot(totalNEI$year, ((totalNEI$Emissions)/10^6), type = "o", col = "red", 
     main = expression("Emissoes PM2.5 por Ano nos EUA"), ylab = expression("Emissoes nos EUA de PM2.5 (10^6 Toneladas)"), xlab = "Ano")
#close and save png file
dev.off()

#Open second device png
png("Plot1b.png", width=480, height=480)
barplot(
    ((totalNEI$Emissions)/10^6),
    col=rgb(0.5,0.1,0.6,0.6),
    totalNEI$year,
    xlab="Ano",
    ylab="Emissoes nos EUA de PM2.5 (10^6 Toneladas)",
    main="Emissoes PM2.5 por Ano nos EUA"
)
#Close and save device png
dev.off()

totalNEI[with(totalNEI, order(-Emissions)), ]

