#This script sets up the environment for all scripts in the /Scripts folder

#### LOAD PACKAGES ----
require(corrplot)
require(data.table)
require(dplyr)
require(evolqg)
require(evolvability)
require(ggplot2)
require(grDevices)
require(gridExtra)
require(purrr)
require(data.table)
require(Hmisc)
require(MCMCglmm)
require(paletteer)
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

col.category = c("#F3F1B0", #autozooid
                 "#8F475E", #avicularia
                 "#ACF3B0") #ovicell

#Formation colors
#oldest to youngest
col.form <- paletteer_d("colorBlindness::paletteMartin", n = 15)

# "Nukumaru Limestone"  = col.form[[2]] #004949FF
# "Nukumaru Brown Sand" = col.form[[3]] #009292FF
# "Tewkesbury Formation" = col.form[[4]] #FF6DB6FF
# "Butlers Shell Conglomerate" = col.form[[5]] #FFB6DBFF
# "Lower Kai-iwi Shellbed" = col.form[[6]] #490092FF
# "Upper Westmere Shellbed" = col.form[[7]] #006DDBFF
# "Upper Kai-iwi Shellbed" = col.form[[8]] #B66DFFFF
# "Lower Castlecliff Shellbed" = col.form[[9]] #6DB6FFFF
# "Tainui Shellbed" = col.form[[10]] #B6DBFFFF
# "Shakespeare Cliff Basal Sand Shellbed" = col.form[[11]] #FFFF6DFF
# "Upper Castlecliff Shellbed" = col.form[[12]] #920000FF
# "modern" = col.form[[13]] #DB6D00FF

agon.col.form <- c(col.form[[2]], #"Nukumaru Limestone"
                   col.form[[3]], #"Nukumaru Brown Sand"
                   col.form[[4]], #"Tewkesbury Formation"
                   col.form[[6]], #"Lower Kai-iwi Shellbed"
                   col.form[[8]], #"Upper Kai-iwi Shellbed"
                   col.form[[9]], #"Lower Castlecliff Shellbed"
                   col.form[[10]], #"Tainui Shellbed"
                   col.form[[11]], #"Shakespeare Cliff Basal Sand Shellbed"
                   col.form[[12]], #"Upper Castlecliff Shellbed"
                   col.form[[13]]) #"modern"

disc.col.form <- c(col.form[[2]], #"Nukumaru Limestone"
                   col.form[[3]], #"Nukumaru Brown Sand"
                   col.form[[4]], #"Tewkesbury Formation"
                   col.form[[5]], #"Butlers Shell Conglomerate"
                   col.form[[6]], #"Lower Kai-iwi Shellbed"
                   col.form[[8]], #"Upper Kai-iwi Shellbed"
                   col.form[[10]], #"Tainui Shellbed"
                   col.form[[13]]) #"modern"

int.col.form <- c(col.form[[9]], #"Lower Castlecliff Shellbed"
                  col.form[[10]], #"Tainui Shellbed"
                  col.form[[11]], #"Shakespeare Cliff Basal Sand Shellbed"
                  col.form[[13]]) #"modern"

spec.col.form <- c(col.form[[7]], #"Upper Westmere Shellbed"
                   col.form[[8]], #"Upper Kai-iwi Shellbed"
                   col.form[[9]], #"Lower Castlecliff Shellbed"
                   col.form[[10]], #"Tainui Shellbed"
                   col.form[[11]], #"Shakespeare Cliff Basal Sand Shellbed"
                   col.form[[12]], #"Upper Castlecliff Shellbed"
                   col.form[[13]]) #"modern"


