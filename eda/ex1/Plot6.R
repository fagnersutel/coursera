setwd('~/OneDrive/Cursos/Coursera/DataScienceSpecialization/coursera/eda/ex1/')
summarySCC_PM25 <- readRDS("FNEI_data/summarySCC_PM25.rds")
Source_Classification_Code <- readRDS("FNEI_data/Source_Classification_Code.rds")

#Criamos o subconjunto de dados / Create a subset
cidade_veiculos <- subset(summarySCC_PM25, summarySCC_PM25$fips %in% c("24510", "06037") & summarySCC_PM25$type == "ON-ROAD")

#Fazemos o agregado de emissoes por ano por ano - cidade / Agreggate the data emissions by year and city
veiculos <- aggregate(Emissions ~ year + fips, cidade_veiculos, sum)
par(mfrow=c(1,2))
#Primeiro plot de linhas / First plot by lines
library(ggplot2)
ggplot(veiculos, aes(year, Emissions, col=(fips))) +
    geom_line() +
    geom_point(col = "magenta") +
    ggtitle(expression("Emissoes Anuais de PM2.5 por Veiculos Automotores em Baltimore e Los Angeles"))+
    theme(legend.title = element_text(face = "bold")) +
    ylab(expression("Emissoes de PM2.5 (Toneladas)")) +
    xlab("Ano") +
    scale_colour_discrete(name = "Fontes") 

#Grafico de barras / barplot
ggplot(veiculos,aes(factor(year),Emissions)) +
    geom_bar(stat="identity",fill="#48D1CC",width=0.75) +
    theme_bw() +  guides(fill=FALSE) +
    labs(x="Ano", y=expression("Emissoes de PM2.5 (Toneladas)")) + 
    labs(title=expression("Emissoes Anuais de PM2.5 por Veiculos Automotores em Baltimore e Los Angeles"))
    

#Criamos o device para gerar um arquivo png / Create a device to obtain a png file
png("Plot6.png", width=480, height=480)
ggplot(veiculos, aes(year, Emissions)) +
    geom_line(col = "#085BD3") +
    geom_point(col = "orange") +
    ggtitle(expression("Emissoes de PM2.5 por Ano em Baltimore por Veiculos Automotores:)"))+
    theme(legend.title = element_text(face = "bold")) +
    ylab(expression("Emissoes de PM2.5 (Toneladas)")) +
    xlab("Ano") +
    scale_colour_discrete(name = "Fontes") 

dev.off()

#Criamos o segundo device para gerar um arquivo png B/ Create a second device to obtain a png file B
png("Plot6b.png", width=480, height=480)
ggplot(veiculos,aes(factor(year),Emissions)) +
    geom_bar(stat="identity",fill="#48D1CC",width=0.75) +
    theme_bw() +  guides(fill=FALSE) +
    labs(x="Ano", y=expression("Emissoes de PM2.5 (Toneladas)")) + 
    labs(title=expression("Emissoes de PM2.5 por Ano em Baltimore por Veiculos Automotores:"))

dev.off()

