setwd('~/OneDrive/Cursos/Coursera/DataScienceSpecialization/coursera/eda/ex1/')
summarySCC_PM25 <- readRDS("FNEI_data/summarySCC_PM25.rds")
Source_Classification_Code <- readRDS("FNEI_data/Source_Classification_Code.rds")

#Criamos o subconjunto de dados / Create a subset
cidade_veiculos <- subset(summarySCC_PM25, summarySCC_PM25$fips %in% c("24510", "06037") & summarySCC_PM25$type == "ON-ROAD")
cidade_veiculos$fips = ifelse(cidade_veiculos$fips == "24510", "Baltimore", "Los Angeles")
#Fazemos o agregado de emissoes por ano por ano - cidade / Agreggate the data emissions by year and city
veiculos <- aggregate(Emissions ~ year + fips, cidade_veiculos, sum)
par(mfrow=c(1,2))
#Primeiro plot de linhas / First plot by lines
library(ggplot2)
ggplot(veiculos, aes(year, Emissions, col=(fips))) +
    geom_line() +
    geom_point(col = "magenta") +
    ggtitle(expression("Emissoes Anuais de PM2.5 por Veiculos em Baltimore e Los Angeles"))+
    theme(legend.title = element_text(face = "bold")) +
    ylab(expression("Emissoes de PM2.5 (Toneladas)")) +
    xlab("Ano") +
    scale_colour_discrete(name = "Fontes") 

#Grafico de barras / barplot
#Não esta ajustando ano inteiro, ver: 
ggplot(cidade_veiculos, aes(x=year, y=Emissions, fill=fips)) +
    geom_bar(aes(fill=year),stat="identity") +
    facet_grid(.~fips) +
    guides(fill=FALSE) + theme_bw() +
    labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
    labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))


#Criamos o device para gerar um arquivo png / Create a device to obtain a png file
png("Plot6.png", width=480, height=480)
ggplot(veiculos, aes(year, Emissions, col=(fips))) +
    geom_line() +
    geom_point(col = "magenta") +
    ggtitle(expression("Emissoes Anuais de PM2.5 por Veiculos em Baltimore e Los Angeles"))+
    theme(legend.title = element_text(face = "bold")) +
    ylab(expression("Emissoes de PM2.5 (Toneladas)")) +
    xlab("Ano") +
    scale_colour_discrete(name = "Fontes") 

dev.off()

#Criamos o segundo device para gerar um arquivo png B/ Create a second device to obtain a png file B
png("Plot6b.png", width=480, height=480)
ggplot(cidade_veiculos, aes(x=year, y=Emissions, fill=fips)) +
    geom_bar(aes(fill=year),stat="identity") +
    facet_grid(.~fips) +
    guides(fill=FALSE) + theme_bw() +
    labs(x="Anos", y=expression("Emissoes de PM2.5 (Toneladas)")) + 
    labs(title=expression("PM"[2.5]*" Emissoes Anuais de PM2.5 por Veiculos em Baltimore e Los Angeles"))
dev.off()
    
