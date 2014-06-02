#Plot 1
# Let's load the given data set's 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# To plot the total emissions, we need to sum all the observations of Emissions by Year. I used Aggregate() function to calculate the Total #Emissions

total_emissions <- aggregate(Emissions ~ year, data = NEI, sum, na.rm=TRUE)
#Let's capture the plot using the png() Graphics Device.

png("plot1.png", height = 600, width = 480)
barplot((a$Emissions/1000000),a$year,names.arg = c("1999","2002","2005","2008"), # scaled down the Emissions
        col = "steelblue",xlab = "Years",
        ylab = expression("Total "* PM[2.5]*" Emissions in Mega-Tons"),
        main = expression("Emissions of  "* PM[2.5]*" by each year(all sources)"))
dev.off()

#Plot 2
# Let's load the given data set's 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Here we need to find the total emissions in the Baltimore City, Maryland. So lets subset only the data with fips == "24510".
baltimore = subset(NEI, NEI$fips == 24510)
# To plot the total emissions, we need to sum all the observations of Emissions by Year. I used Aggregate() function to calculate the Total #Emissions.
total_emissions_baltimore <- aggregate(Emissions ~ year, data = baltimore, sum, na.rm=TRUE)
#Let's capture the plot using the png() Graphics Device.
png("plot2.png", height = 600, width = 600)
barplot(total_emissions_baltimore$Emissions,total_emissions_baltimore$year,
        names.arg = c("1999","2002","2005","2008"),col = "steelblue",
        xlab = "Years",ylab = expression("Total "* PM[2.5]*" Emissions in Tons"),
        main = expression("Emissions of  "* PM[2.5]*" by each year in Baltimore City, Maryland (all sources)"))
dev.off()

#plot 3
# Let's load the given data set's 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
# Here we need to find the total emissions in the Baltimore City, Maryland. So lets subset only the data with fips == "24510".
baltimore = subset(NEI, NEI$fips == 24510)
# To plot the total emissions, we need to sum all the observations of Emissions by Year. I used Aggregate() function to calculate the Total #Emissions.
total_emissions_baltimore_type <- aggregate(Emissions ~ year+ type, data = baltimore, sum, na.rm=TRUE)
#Let's capture the plot using the png() Graphics Device.
png("plot3.png", height = 300, width = 800)cc
qplot(year, Emissions, data = total_emissions_baltimore, facets = .~type) +
        scale_x_continuous("Year", breaks = total_emissions_baltimore$year) + 
        labs(title = expression("Total "* PM[2.5]*" Emissions by each Year and Type in Baltimore"))
dev.off()

#Plot 4
# Let's load the given data set's 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#So we need only data from coal cumbustion related sources.
coal <- SCC[grepl("Coal",SCC$EI.Sector),]$SCC
total_coal <- subset(NEI, NEI$SCC %in% coal)
total_coal_aggregate = aggregate(Emissions ~ year, data = total_coal, sum, na.rm = TRUE)
#Let's capture the plot using the png() Graphics Device.
png("plot4.png", height = 300, width = 800)
barplot((total_coal_aggregate$Emissions/1000), names.arg = c("1999","2002","2005","2008"), 
            col = "steelblue",xlab = "Years",ylab = expression("Total "* PM[2.5]*" Emissions in Kilo-tons"),
            main = expression("Emissions of  "* PM[2.5]*" by each year for Coal Cumbustion related Sources"))
dev.off()

#Plot 5
# Let's load the given data set's 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#So we need data from only Vehicles as sources.
vehicles <- SCC[grepl("Vehicles",SCC$EI.Sector),]$SCC
#Now subset the NEI data and then subset to get only Baltimore region data based on the sources we just got from the above code.
total_vehicles <- subset(NEI, NEI$SCC %in% vehicles)
total_vehicles_baltimore <- subset(total_vehicles, total_vehicles$fips == 24510)
total_vehicles_baltimore_year <- aggregate(Emissions ~ year, data = total_vehicles_baltimore, sum, na.rm=TRUE)
# Lets capture the plot using the png() Graphic device.
png("plot5.png" heigt = 400, width = 800)
barplot(total_vehicles_baltimore_year$Emissions, names.arg = c("1999","2002","2005","2008"), 
          col = "steelblue",xlab = "Years",ylab = expression("Total "* PM[2.5]*" Emissions tons"),
          main = expression("Emissions of  "* PM[2.5]*" by each year from motor vehicle Sources in Baltimore"))
dev.off()

#Plot 6
# Let's load the given data set's 
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
vehicles <- SCC[grepl("Vehicles",SCC$EI.Sector),]$SCC
total_vehicles <- subset(NEI, NEI$SCC %in% vehicles)
total_vehicles_baltimore <- subset(total_vehicles, total_vehicles$fips == 24510)
total_vehicles_la <- subset(total_vehicles, total_vehicles$fips == "06037")
total_vehicles_baltimore_year <- aggregate(Emissions ~ year, data = total_vehicles_baltimore, sum, na.rm=TRUE)
total_vehicles_la_year <- aggregate(Emissions ~ year, data = total_vehicles_la, sum, na.rm=TRUE)

png("plot6.png", height = 800, width = 480)
par(mfrow = c(2,1))
barplot(total_vehicles_baltimore_year$Emissions, names.arg = c("1999","2002","2005","2008"), 
                  col = "steelblue",xlab = "Years",ylab = expression("Total "* PM[2.5]*" Emissions tons"),
                  main = expression("Emissions of  "* PM[2.5]*" by each year from motor vehicle in Baltimore"))
barplot(total_vehicles_la_year$Emissions, names.arg = c("1999","2002","2005","2008"), 
                  col = "steelblue",xlab = "Years",ylab = expression("Total "* PM[2.5]*" Emissions tons"),
                  main = expression("Emissions of  "* PM[2.5]*" by each year from motor vehicle Los Angeles"))
dev.off()