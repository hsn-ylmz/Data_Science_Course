library(dplyr)
library(ggplot2)


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

data <- left_join(NEI, SCC, by = "SCC")

vehicle <- data[mapply(grepl, pattern = 'Vehicle', x = data$SCC.Level.Two),]
balt.vehic <- vehicle %>% filter(fips == '24510') %>% 
        group_by(year) %>% summarise(sum = sum(Emissions))


png('Plot5.png', width = 480, height = 480)

balt.vehic %>% ggplot(aes(x = year, y = sum)) +
        geom_line(color = 'grey40', size = 1.1) +
        geom_point(color = 'black', size = 2) +
        labs(x = 'Years', y = 'Total Emission (ton)', 
             title = 'Total Emissions of Vehicles in Baltimore')

dev.off()