library(dplyr)
library(ggplot2)
library(patchwork)


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

data <- left_join(NEI, SCC, by = "SCC")

vehicle <- data[mapply(grepl, pattern = 'Vehicle', x = data$SCC.Level.Two),]

balt.vehic <- vehicle %>% filter(fips == '24510') %>% 
        group_by(year) %>% summarise(sum = sum(Emissions)) %>% 
        mutate(county = 'Baltimore')
ange.vehic <- vehicle %>% filter(fips == '06037') %>%
        group_by(year) %>% summarise(sum = sum(Emissions)) %>%
        mutate(county = 'Los Angeles')

balt.angeles <- bind_rows(balt.vehic, ange.vehic) %>% group_by(year, county)

png('Plot5.png', width = 480, height = 480)

plot1 <- balt.vehic %>% 
        ggplot(aes(x = year, y = sum)) +
        geom_line(size = 1.1, color = 'coral1') +
        geom_point(size = 2, color = 'coral1') +
        labs(x = 'Years', y = 'Total Emission (ton)', 
             title = 'Total Emissions of Vehicles in Baltimore')
plot2 <- ange.vehic %>% 
        ggplot(aes(x = year, y = sum)) +
        geom_line(size = 1.1, color = 'turquoise3') +
        geom_point(size = 2, color = 'turquoise3') +
        labs(x = 'Years', y = 'Total Emission (ton)', 
             title = 'Total Emissions of Vehicles in Los Angeles')
plot3 <- balt.angeles %>% 
        ggplot(aes(x = year, y = sum, group = county, color = county)) +
        geom_line(size = 1.1) +
        geom_point(size = 2) +
        labs(x = 'Years', y = 'Total Emission (ton)', 
             title = 'Total Emissions of Vehicles')

plot1 + plot2 + plot3

dev.off()

