setwd('~/OneDrive/Cursos/Coursera/DataScienceSpecialization/coursera/eda/ex1/')
list.files()
par(mfrow=c(1,2))
summarySCC_PM25 <- readRDS("FNEI_data/summarySCC_PM25.rds")
Source_Classification_Code <- readRDS("FNEI_data/Source_Classification_Code.rds")

#Criamos o subconjunto de dados / Create a subset
city <- subset(summarySCC_PM25, summarySCC_PM25$fips == "24510")

#Fazemos o agregado de emissoes por ano por ano - cidade / Agreggate the data emissions by year and city
totalcity <- aggregate(Emissions ~ year, city, sum)

#Primeiro plot de linhas / First plot by lines
plot(totalcity$year, totalcity$Emissions, type = "o", main = expression("Emissoes de PM2.5 por Ano em Baltimore"), 
     xlab = "Ano", ylab = expression("Emissoes de PM2.5 (Toneladas)"), col = "springgreen3")

#Grafico de barras / barplot
barplot(totalcity$Emissions, names.arg=totalcity$year, xlab="Ano", ylab="Emissoes de PM2.5 (Toneladas)",
    main="Emissoes de PM2.5 por Ano em Baltimore"
)

#Criamos o device para gerar um arquivo png / Create a device to obtain a png file
png("Plot2.png", width=480, height=480)
plot(totalcity$year, totalcity$Emissions, type = "o", main = expression("Emissoes de PM2.5 por Ano em Baltimore"), 
     xlab = "Ano", ylab = expression("Emissoes de PM2.5 (Toneladas)"), col = "springgreen3")

dev.off()
#Criamos o segundo device para gerar um arquivo png B/ Create a second device to obtain a png file B
png("Plot2b.png", width=480, height=480)
barplot(totalcity$Emissions, names.arg=totalcity$year, xlab="Ano", ylab="Emissoes de PM2.5 (Toneladas)",
        main="Emissoes de PM2.5 por Ano em Baltimore"
)
dev.off()
