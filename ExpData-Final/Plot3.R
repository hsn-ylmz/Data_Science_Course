library(dplyr)
library(ggplot2)


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

sum.balt.type <- NEI %>% filter(fips == '24510') %>% 
        group_by(type, year) %>% summarise(sum = sum(Emissions))

png('Plot3.png', width = 480, height = 480)

sum.balt.type %>%
ggplot(aes(x = year, y = sum, group = type, color = type)) + 
        geom_line(size = 1.2) + 
        geom_point(size = 3) +
        labs(x = 'Years', y = 'Total Emission', 
             title = 'Total Emissions of Baltimore')

dev.off()
