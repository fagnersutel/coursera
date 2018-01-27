setwd('~/OneDrive/Cursos/Coursera/DataScienceSpecialization/coursera/eda/ex1/')
list.files()
summarySCC_PM25 <- readRDS("FNEI_data/summarySCC_PM25.rds")
Source_Classification_Code <- readRDS("FNEI_data/Source_Classification_Code.rds")

#Criamos o subconjunto de dados / Create a subset
city <- subset(summarySCC_PM25, summarySCC_PM25$fips == "24510")

#Fazemos o agregado de emissoes por ano por ano - cidade / Agreggate the data emissions by year and city
origem <- aggregate(Emissions ~ year + type, city, sum)
par(mfrow=c(1,2))
#Primeiro plot de linhas / First plot by lines
library(ggplot2)
ggplot(origem, aes(year, Emissions, col = type)) +
    geom_line() +
    geom_point() +
    ggtitle(expression("Emissoes de PM2.5 por Ano em Baltimore por Origem)"))+
    theme(legend.title = element_text(face = "bold")) +
    ylab(expression("Emissoes de PM2.5 (Toneladas)")) +
    xlab("Ano") +
    scale_colour_discrete(name = "Fontes") 

#Grafico de barras / barplot
ggplot(city,aes(factor(year),Emissions,fill=type)) +
    facet_grid(.~type,scales = "free",space="free") + 
    labs(x="Ano", y=expression("Emissoes de PM2.5 (Toneladas)")) + 
    labs(title=expression("Emissoes de PM2.5 por Ano em Baltimore por Origem")) +
    geom_bar(stat="identity") +
    theme_bw() + guides(fill=FALSE)
    

#Criamos o device para gerar um arquivo png / Create a device to obtain a png file
png("Plot3.png", width=480, height=480)
ggplot(origem, aes(year, Emissions, col = type)) +
    geom_line() +
    geom_point() +
    ggtitle(expression("Emissoes de PM2.5 por Ano em Baltimore por Origem)"))+
    theme(legend.title = element_text(face = "bold")) +
    ylab(expression("Emissoes de PM2.5 (Toneladas)")) +
    xlab("Ano") +
    scale_colour_discrete(name = "Fontes") 

dev.off()

#Criamos o segundo device para gerar um arquivo png B/ Create a second device to obtain a png file B
png("Plot3b.png", width=480, height=480)
ggplot(city,aes(factor(year),Emissions,fill=type)) +
    facet_grid(.~type,scales = "free",space="free") + 
    labs(x="Ano", y=expression("Emissoes de PM2.5 (Toneladas)")) + 
    labs(title=expression("Emissoes de PM2.5 por Ano em Baltimore por Origem")) +
    geom_bar(stat="identity") +
    theme_bw() + guides(fill=FALSE)

dev.off()
