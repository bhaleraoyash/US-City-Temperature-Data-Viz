suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(rgdal))
library(broom)
library(magrittr)
library(dplyr)
library(circlize)
library(ggplot2)
library(tidyr)
library(lubridate)
library(fs)
library(janitor)
library(cowplot)

# Read data from the csv files for each city and 
# call the plotGraph function that will the return the graph.
boston_data <- read_csv("boston.csv", col_types = cols())
boston_graph <- plotGraph(boston_data, "Boston (MA)")

miami_data <- read_csv("miami.csv", col_types = cols())
miami_graph <- plotGraph(miami_data, "Miami (FL)")

nyc_data <- read_csv("nyc.csv", col_types = cols())
nyc_graph <- plotGraph(nyc_data, "New York City (NY)")

seattle_data <- read_csv("seattle.csv", col_types = cols())
seattle_graph <- plotGraph(seattle_data, "Seattle (WA)")

#Plot the graphs on 2*2 grid and save to pdf.
plot_grid(boston_graph, miami_graph, nyc_graph, seattle_graph, ncol = 2, nrow = 2)
ggsave("US City Temperature.pdf", width = 10, height = 6, unit = "in")

#Function that takes data from csv and name of city as input and returns the graph.
plotGraph <- function(data, nameOfCity){
  data <- as_tibble(data)
  
  data <- select(data, Date, Max.TemperatureF, Mean.TemperatureF, Min.TemperatureF) %>%
    filter(Date >= "2015-01-01", Date <= "2015-12-31") %>%
    mutate(daynumber = yday(Date)) %>%
    clean_names()
  
  day <- group_by(data, date, daynumber) %>%
    summarise(avg_temp = mean(mean_temperature_f, na.rm = TRUE),
              min_temp = mean(min_temperature_f, na.rm = TRUE),
              max_temp = mean(max_temperature_f, na.rm = TRUE))
  
  day <- mutate(day, daynumber = as_date(daynumber, origin = "2014-12-31"))
  
  col_temp <- c("#4166F5", "#FFFFFF", "#FFA500")
  
  result <- ggplot(day) + 
    geom_linerange(aes(daynumber,
                       ymin = min_temp,
                       ymax = max_temp,
                       colour = avg_temp),
                   size = 0.4,
                   alpha = 1.0) + 
    scale_y_continuous(breaks = seq(20, 100, 20), 
                       limits = c(-10, 100),
                       expand = expansion(),
                       labels = c("20F", "40F", "60F", "80F", "100F")) +
    scale_colour_gradientn(colours = col_temp, 
                           limits = c(-10, 100), 
                           breaks = seq(0, 90, 25)) +
    coord_polar() +
    labs(x = " ", title = {{nameOfCity}}, colour = "Temperature") +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(size = 0.04, 
                                          linetype = 'solid',
                                          colour = "black"),
          panel.grid.minor = element_blank())
  
  return (result)
}