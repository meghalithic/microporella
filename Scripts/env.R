#This script sets up the environment for all scripts in the /Scripts folder

#### LOAD PACKAGES ----
require(data.table)
require(dplyr)
require(ggplot2)
require(purrr)
require(data.table)
require(Hmisc)
library(scales)
require(splitstackshape)
require(stringr)
#require(rgdal)
#require(rgeos)
#require(geojsonio)
require(geojsonsf)
require(sf)
require(jsonlite)
require(s2)

#### PLOTS ----
plot.theme <- theme(text = element_text(size = 16),
                    legend.position = "none",
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    plot.background = element_rect(fill = 'transparent', color = NA))
