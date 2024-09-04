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
require(MASS)
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

source("./Scripts/norm.vector.funct.R")

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
# "Tewkesbury Formation" = col.form[[1]] #000000FF
# "Butlers Shell Conglomerate" = col.form[[13]] #DB6D00FF
# "Lower Kai-iwi Shellbed" = col.form[[6]] #490092FF
# "Upper Westmere Shellbed" = col.form[[11]] #920000FF
# "Upper Kai-iwi Shellbed" = col.form[[8]] #B66DFFFF
# "Lower Castlecliff Shellbed" = col.form[[9]] #6DB6FFFF
# "Tainui Shellbed" = col.form[[15]] #FFFF6DFF 
# "Shakespeare Cliff Basal Sand Shellbed" = col.from[[12]] #924900FF
# "Upper Castlecliff Shellbed" = col.form[[7]] #006DDBFF
# "Wanganui Core" = col.form[[4]] #FF6DB6FF
# "modern" = col.form[[5]] #FFB6DBFF

agon.col.form <- c("#004949FF", #"Nukumaru Limestone"
                   "#009292FF", #"Nukumaru Brown Sand"
                   "#490092FF", #"Lower Kai-iwi Shellbed"
                   "#B66DFFFF", #"Upper Kai-iwi Shellbed"
                   "#6DB6FFFF", #"Lower Castlecliff Shellbed"
                   "#FFFF6DFF", #"Tainui Shellbed"
                   "#924900FF", #"Shakespeare Cliff Basal Sand Shellbed"
                   "#FF6DB6FF", #"Whanganui Core"
                   "#FFB6DBFF") #"modern"

disc.col.form <- c("#004949FF", #"Nukumaru Limestone"
                   "#009292FF", #"Nukumaru Brown Sand"
                   "#490092FF", #"Lower Kai-iwi Shellbed"
                   '#B66DFFFF', #"Upper Kai-iwi Shellbed"
                   "#FFFF6DFF", #"Tainui Shellbed"
                   "#FFB6DBFF") #"modern"

int.col.form <- c("#6DB6FFFF", #"Lower Castlecliff Shellbed"
                  "#FF6DB6FF", #"Whanganui Core
                  "#FFB6DBFF") #"modern"

spec.col.form <- c("#B66DFFFF", #"Upper Kai-iwi Shellbed"
                   "#6DB6FFFF", #"Lower Castlecliff Shellbed"
                   "#FFFF6DFF", #"Tainui Shellbed"
                   "#924900FF", #"Shakespeare Cliff Basal Sand Shellbed"
                   "#FFB6DBFF") #"modern"

agon_form <- c("Nukumaru Limestone",
               "Nukumaru Brown Sand",
               "Lower Kai-iwi Shellbed",
               "Upper Kai-iwi Shellbed",
               "Lower Castlecliff Shellbed",
               "Tainui Shellbed",
               "Shakespeare Cliff Basal Sand Shellbed",
               "Whanganui Core",
               "modern")
agon_form <- factor(agon_form, 
                    levels = c("Nukumaru Limestone",
                               "Nukumaru Brown Sand",
                               "Lower Kai-iwi Shellbed",
                               "Upper Kai-iwi Shellbed",
                               "Lower Castlecliff Shellbed",
                               "Tainui Shellbed",
                               "Shakespeare Cliff Basal Sand Shellbed",
                               "Whanganui Core",
                               "modern"))

disc_form <- c("Nukumaru Limestone",
               "Nukumaru Brown Sand",
               "Lower Kai-iwi Shellbed",
               "Upper Kai-iwi Shellbed",
               "Tainui Shellbed",
               "modern")
disc_form <- factor(disc_form,
                    levels = c("Nukumaru Limestone",
                               "Nukumaru Brown Sand",
                               "Lower Kai-iwi Shellbed",
                               "Upper Kai-iwi Shellbed",
                               "Tainui Shellbed",
                               "modern"))

int_form <- c("Lower Castlecliff Shellbed",
              "Whanganui Core",
              "modern")
int_form <- factor(int_form,
                   levels = c("Lower Castlecliff Shellbed",
                              "Whanganui Core",
                              "modern"))

spec_form <- c("Upper Kai-iwi Shellbed",
               "Lower Castlecliff Shellbed",
               "Tainui Shellbed",
               "Shakespeare Cliff Basal Sand Shellbed",
               "modern")
spec_form <- factor(spec_form,
                    levels = c("Upper Kai-iwi Shellbed",
                               "Lower Castlecliff Shellbed",
                               "Tainui Shellbed",
                               "Shakespeare Cliff Basal Sand Shellbed",
                               "modern"))

agon_form_trans <- c("NKLS to NKBS", 
                     "NKBS to LKI",
                     "LKI to UKI",
                     "UKI to LCSB", 
                     "LCSB to TSB",
                     "TSB to SHC", 
                     "SHC to WC",
                     "WC to modern",
                     "")
agon_form_trans <- factor(agon_form_trans,
                              levels = c("NKLS to NKBS", 
                                         "NKBS to LKI",
                                         "LKI to UKI",
                                         "UKI to LCSB", 
                                         "LCSB to TSB",
                                         "TSB to SHC", 
                                         "SHC to WC",
                                         "WC to modern",
                                         ""))

disc_form_trans <- c("NKLS to NKBS", 
                     "NKBS to LKI",
                     "LKI to UKI",
                     "UKI to TSB",
                     "TSB to modern",
                     "")
disc_form_tran <- factor(disc_form_trans,
                         levels = c("NKLS to NKBS", 
                                    "NKBS to LKI",
                                    "LKI to UKI",
                                    "UKI to TSB",
                                    "TSB to modern",
                                    ""))

int_form_trans <- c("LCSB to WC", 
                    "WC to modern",
                    "")
int_form_trans <- factor(int_form_trans,
                         levels = c("LCSB to WC",
                                    "WC to modern",
                                    ""))

spec_form_trans <- c("UKI to LCSB", 
                     "LCSB to TSB",
                     "TSB to SHC", 
                     "SHC to modern",
                     "")
spec_form_trans <- factor(spec_form_trans,
                          levels = c("UKI to LCSB", 
                                     "LCSB to TSB",
                                     "TSB to SHC", 
                                     "SHC to modern",
                                     ""))
