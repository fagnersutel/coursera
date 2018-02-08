
# Read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Change to character from factors
NEI$SCC <- as.character(NEI$SCC)
SCC$SCC <- as.character(SCC$SCC)


# Q1
# set up graphical device
#png(filename = "plot1.png", width = 480, height = 480)

# turn off scientific notation
options(scipen=999) 

# create summarized data for plot
Total_Em <- NEI %>% group_by(year) %>% summarize(total = sum(Emissions))

# Plot Total_Em
plot(Total_Em, xlab = "Year", ylab = "Emissions (tons)", pch = 1, main = "PM2.5 Emissions by Year")

# Add text at top of bars
text(x = Total_Em$year, y = Total_Em$total, labels = paste(format(Total_Em$total, big.mark = ";"), sep = ""), 
     pos = c(4,3,3,2), cex = 0.8, col = "blue")

# close device
#dev.off()



# Change to character from factors
NEI$SCC <- as.character(NEI$SCC)
SCC$SCC <- as.character(SCC$SCC)


# Q2
# set up graphical device
#png(filename = "plot2.png", width = 480, height = 480)

# turn off scientific notation
options(scipen=999) 

# create summarized data for plot
Total_Em_Bm <- NEI %>% filter(fips=="24510") %>% group_by(year) %>% summarize(total = sum(Emissions))

# Plot Total_Em
plot(Total_Em_Bm, xlab = "Year", ylab = "Emissions (tons)", pch = 1, main = "PM2.5 Emissions by Year in Baltimore")

# Add text at top of bars
text(x = Total_Em_Bm$year, y = Total_Em_Bm$total, labels = paste(format(Total_Em_Bm$total, big.mark = ","), sep = ""), 
     pos = c(4,2), cex = 0.8, col = "blue")

# close device
#dev.off()


# Change to character from factors
NEI$SCC <- as.character(NEI$SCC)
SCC$SCC <- as.character(SCC$SCC)


# Q3 
# set up graphical device
#png(filename = "plot3.png", width = 480, height = 480)

# turn off scientific notation
options(scipen=999)

# create summarized data for plot
Total_Em_Bm_Ty <- NEI %>% filter(fips=="24510") %>% group_by(type, year) %>% summarize(total = sum(Emissions))

# Plot Total_Em
bmore_plot <- ggplot(Total_Em_Bm_Ty, aes(year, total, color = type)) + geom_point()
bmore_plot + labs(x = "Year", y = "Emissions", title = "PM2.5 Emissions by Year", subtitle = "Baltimore City")

# close device
#dev.off()

# Change to character from factors
NEI$SCC <- as.character(NEI$SCC)
SCC$SCC <- as.character(SCC$SCC)


#Q4
# set up graphical device
#png(filename = "plot4.png", width = 480, height = 480)

# turn off scientific notation
options(scipen=999)

# filter for only coal related entries
SCC_Coal <- SCC[grep("Coal", SCC$EI.Sector), c("SCC", "Short.Name", "EI.Sector")]

# create summarized data for plot
Total_Em_Coal <- inner_join(NEI, SCC_Coal, by = "SCC") %>% group_by(year) %>% summarize(total = sum(Emissions))

# Plot Coal
coal_plot <- ggplot(Total_Em_Coal, aes(year, total)) + geom_point()
coal_plot + labs(x = "Year", y = "Emissions (tons)", title = "PM2.5 Emissions by Year", subtitle = "Coal Combustion Sources")

#dev.off()


#Q6
# set up graphical device
#png(filename = "plot6.png", width = 480, height = 480)

# filter for only motor vehicle source
SCC_OnRoad <- SCC[grep("Mobile - On-Road", SCC$EI.Sector), c("SCC", "Short.Name", "EI.Sector")]

# create summarized data for plot
Total_Em_OnRoad <- inner_join(NEI, SCC_OnRoad, by = "SCC") %>% filter(fips %in% c("24510", "06037")) %>% 
  group_by(fips, year) %>% summarize(total = sum(Emissions)) %>% arrange(fips)

Total_Em_OnRoad <- cbind(Total_Em_OnRoad, City = c(rep("Los Angeles",4),rep("Baltimore",4)))

# Plot motor vehicles for LA and Baltimore
coal_plot <- ggplot(Total_Em_OnRoad, aes(year, total, color = City)) + geom_point()
coal_plot + labs(x = "Year", y = "Emissions (tons)", title = "PM2.5 Emissions by Year", subtitle = "Motor Vehicles")

#dev.off()





#Q5
# set up graphical device
#png(filename = "plot5.png", width = 480, height = 480)

# turn off scientific notation
options(scipen=999)

# filter for only motor vehicle source
SCC_OnRoad <- SCC[grep("Mobile - On-Road", SCC$EI.Sector), c("SCC", "Short.Name", "EI.Sector")]

# create summarized data for plot
Total_Em_OnRoad <- inner_join(NEI, SCC_OnRoad, by = "SCC") %>% filter(fips == "24510") %>% 
  group_by(year) %>% summarize(total = sum(Emissions))

# Plot Coal
coal_plot <- ggplot(Total_Em_OnRoad, aes(year, total)) + geom_point()
coal_plot + labs(x = "Year", y = "Emissions (tons)", title = "PM2.5 Emissions by Year", subtitle = "Motor Vehicles")

#dev.off()