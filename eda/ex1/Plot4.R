setwd('~/OneDrive/Cursos/Coursera/DataScienceSpecialization/coursera/eda/ex1/')
list.files()
#Carrega os dados de PM2.5
#Load de data about PM2.5
summarySCC_PM25 <- readRDS("FNEI_data//summarySCC_PM25.rds")
#Carrega o c??digo de classificacao de tipos de PM2.5
#Load the Source Classification Code of PM2.5
Source_Classification_Code <- readRDS("FNEI_data//Source_Classification_Code.rds")

#Buscamos no nome curto aqueles registros que possuem o valor "coal" = carvao, ignorando caixa alta e baixa
#Search in short.name rows with value "coal" with case insentitive filter
Source_Classification_CodeCoal <- Source_Classification_Code[grepl("coal", Source_Classification_Code$Short.Name, ignore.case = T),]
#Classifica os dados 
#aqui summarySCC_PM25Coal tem todos os registros poss??veis totalizando 6497651
#Here summarySCC_PM25Coal has all posible ocurrences totalizing 6497651
summarySCC_PM25Coal <- summarySCC_PM25
#tamanho da base / dim data
dim(summarySCC_PM25Coal)
#Se fizermos um filtro utilizando a mesma base fazendo a interseccao apenas com os dados de Source_Classification_CodeCoal reduzimos para 53400 registros
#If run a filter with the same data executing the intersection exclusively eith the data of Source_Classification_CodeCoal this decreases to 53400 ocurrences
summarySCC_PM25Coal <- summarySCC_PM25[summarySCC_PM25$SCC %in% Source_Classification_CodeCoal$SCC,]
#tamanho da base / dim data
dim(summarySCC_PM25Coal)
#Realizamos a agregacao dos dados por meio da funcao soma
#Realize de aggregate of data by the function sum
ResultadorCarvao <- aggregate(Emissions ~ year + type, summarySCC_PM25Coal, sum)
#Plotamos oa dados em grafico de linhas
#Plot the data in line graph
ggplot(ResultadorCarvao, aes(year,Emissions/10^5, col = type)) +
    ggtitle(expression("Emissoes PM2.5 de Carvao por Ano nos EUA (10^5 Toneladas) 1999 ~ 1998")) +
    xlab("Ano") +
    ylab(expression("Emissao Norte Americana de PM2.5 (10^5 Toneladas)")) +
    scale_colour_discrete(name = "Tipo") +
    theme(legend.title = element_text(face = "bold")) +
    geom_line() +
    geom_point() 

#Plotamos os dados em grafico de barras
#Plot the data em bar chart
ggplot(summarySCC_PM25Coal,aes(factor(year),Emissions/10^5)) +
    geom_bar(stat="identity",fill="#48D1CC",width=0.75) +
    theme_bw() +  guides(fill=FALSE) +
    labs(x="Ano", y=expression("Emissao Norte Americana de PM2.5 (10^5 Toneladas)")) + 
    labs(title=expression("Emissoes PM2.5 de Carvao por Ano nos EUA (10^5 Toneladas) 1999 ~ 1998"))


#Abrimos o device paragerar o arquivo png
#Open device to output png file
png("Plot4.png", width=480, height=480)
#Plot the data
ggplot(ResultadorCarvao, aes(year, Emissions, col = type)) +
    ggtitle(expression("Emissoes PM2.5 de Carvao por Ano nos EUA 1999 ~ 1998")) +
    xlab("Ano") +
    ylab(expression("Emissao Norte Americana de PM2.5 (10^5 Toneladas)")) +
    scale_colour_discrete(name = "Tipo") +
    theme(legend.title = element_text(face = "bold")) +
    geom_line() +
    geom_point() 
#Fechamos o device para fezhar o arquivo e liberar o quartz
#Close device to close the archieve and release the quartz
dev.off()

#Abrimos o device paragerar o arquivo png
#Open device to output png file
png("Plot4b.png", width=480, height=480)
#Plot the data
ggplot(summarySCC_PM25Coal,aes(factor(year),Emissions/10^5)) +
    geom_bar(stat="identity",fill="#48D1CC",width=0.75) +
    theme_bw() +  guides(fill=FALSE) +
    labs(x="Ano", y=expression("Emissao Norte Americana de PM2.5 (10^5 Toneladas)")) + 
    labs(title=expression("Emissoes PM2.5 de Carvao por Ano nos EUA 1999 ~ 1998"))

#Fechamos o device para fezhar o arquivo e liberar o quartz
#Close device to close the archieve and release the quartz
dev.off()

