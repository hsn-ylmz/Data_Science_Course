library(dplyr)


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

sum.baltimore <- NEI %>% filter(fips == '24510') %>% 
        group_by(year) %>% summarise(sum = sum(Emissions))

png('Plot2.png', width = 480, height = 480)

plot(sum.baltimore, type = 'b', lty = 3, lwd = 2, pch = 21,
     main = 'Total Emissions per Year (Baltimore - Maryland)',
     ylab = 'Total Emissions (tons)',
     xlab = 'Year')

dev.off()