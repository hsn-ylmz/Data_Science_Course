library(dplyr)
library(ggplot2)


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

data <- left_join(NEI, SCC, by = "SCC")

coal <- data[mapply(grepl, pattern = 'Combustion', x = data$SCC.Level.One),]
coal <- coal[mapply(grepl, pattern = 'Coal', x = coal$SCC.Level.Three),]

coal.filtered <- NEI %>% group_by(year) %>% summarise(sum = sum(Emissions))

png('Plot4.png', width = 480, height = 480)

coal.filtered %>% ggplot(aes(x = year, y = sum)) +
        geom_line(color = 'grey40', size = 1.1) +
        geom_point(color = 'black', size = 2) +
        labs(x = 'Years', y = 'Total Emission (ton)', 
             title = 'Total Emissions of Coal Combustion Related Sources')

dev.off()
