setwd("../Exploratory Data Analysis/week4/coursera/eda/ex1/")

summarySCC_PM25 <- readRDS("FNEI_data/summarySCC_PM25.rds")
Source_Classification_Code <- readRDS("FNEI_data/Source_Classification_Code.rds")

totalNEI <- aggregate(Emissions ~ year, summarySCC_PM25, sum)
par(mfrow=c(1,2))

plot(totalNEI$year, ((totalNEI$Emissions)/10^6), type = "o", col = "springgreen3", 
     main = expression("Emissoes PM2.5 por Ano nos EUA"), ylab = expression("Emissoes nos EUA de PM2.5 (10^6 Toneladas)"), xlab = "Ano")


barplot(
    ((totalNEI$Emissions)/10^6),
    totalNEI$year,
    xlab="Ano",
    ylab="Emissoes nos EUA de PM2.5 (10^6 Toneladas)",
    main="Emissoes PM2.5 por Ano nos EUA"
)
par(mfrow=c(1,1))

png("Plot1.png", width=480, height=480)
plot(totalNEI$year, ((totalNEI$Emissions)/10^6), type = "o", col = "springgreen3", 
     main = expression("Emissoes PM2.5 por Ano nos EUA"), ylab = expression("Emissoes nos EUA de PM2.5 (10^6 Toneladas)"), xlab = "Ano")
dev.off()

png("Plot1b.png", width=480, height=480)
barplot(
    ((totalNEI$Emissions)/10^6),
    totalNEI$year,
    xlab="Ano",
    ylab="Emissoes nos EUA de PM2.5 (10^6 Toneladas)",
    main="Emissoes PM2.5 por Ano nos EUA"
)
dev.off()

totalNEI[with(totalNEI, order(-Emissions)), ]

