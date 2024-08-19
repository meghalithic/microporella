# The purpose of this code is to:
# 1. test the effects of distortion and tilt on size estimations of zooid type
# compared to within colony variation
## we use 6835 as the example colony
## distortion: this is the comparison of 6835 img1, img2, img3, img4, img5
## tilt: this is the comparison of 6835 img1, img1a, img2
# 2. compare if being picky results in significantly different variation in zooid
# measuarements than running the CLI
## this is the comparison of CLI outputs to Lily's manual outputs of M. intermedia
# 3. test the effect of rotation on estimating zooid categories (NEED TO DO)
## we use 8679 as an example (M.agonistes)
# 4. test methods for detecting errors in ML estimations
# these include testing 
# a) circularity as a way to find pebbles identified as a zooid
# b) using outlier detection to find over and under estimations of a category
# c) using the polygon to find straight lines, indicating that zooids are at the 
# edge of the image

#### ENVIRONMENT ----
require(dplyr)
require(ggplot2)
require(purrr)
require(data.table)
require(Hmisc)
library(scales)

source("Scripts/maha.R")

plot.theme <- theme(text = element_text(size = 16),
                    legend.position = "none",
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    plot.background = element_rect(fill = 'transparent', color = NA))

# WITHIN (Green)
## COL = #009844
## FILL = #c6e4c7

#AMONG (Pink)
## COL = #e8536b
## FILL = #efdeef

#### DATA ----

##### DISTORTION & TILT ------
## these are manually extracted
df.distort <- read.csv(file = "micro_test/6835_KAH_1_box4_distortion_comparisons.csv",
                       header = TRUE)

##### PICKINESS ------
## this is the cli outputs for intermedia
cli.int.list = list.files(path = "Data/CLI-outputs/M.intermedia/",
                      pattern = "\\.csv$",
                      full.names = TRUE,
                      recursive = TRUE)

df.cli.int <- do.call(rbind, lapply(cli.int.list, function(x) fread(x, stringsAsFactors = FALSE)))
nrow(df.cli.int)
length(unique(df.cli.int$image_id)) #126

## these are the boxes to remove based on what lily did
rm.df <- read.csv("Data/Manual-outputs/M.intermedia/image_id_with_object_ids_to_remove.csv",
                  header = TRUE)
length(unique(rm.df$image_id)) #38

## these are Lily's manual outputs of M. intermedia
manual.list = list.files(path = "Data/Manual-outputs/M.intermedia",
                         pattern = "\\.csv$",
                         full.names = TRUE,
                         recursive = TRUE)
manual.list <- manual.list[30:46] #[5:29]

df.man.int <- do.call(rbind, lapply(manual.list, function(x) fread(x, stringsAsFactors = FALSE)))
nrow(df.man.int)
length(unique(df.man.int$image_id)) #25

## this is the metadata
meta.df <- read.csv("Data/Microporella_SEMs_EDM+Mali_05.06.2024.csv",
                    header = TRUE)

##### ROTATION ------
## this is the rotation data
df.top <- read.csv("micro_test/MHR.8679_top.csv",
                   header = TRUE)
df.right <- read.csv("micro_test/MHR.8679_right.csv",
                   header = TRUE)
df.bottom <- read.csv("micro_test/MHR.8679_bottom.csv",
                   header = TRUE)
df.left <- read.csv("micro_test/MHR.8679_left.csv",
                   header = TRUE)

## this is to match boxids
df.rot.ids <- read.csv("micro_test/rotation.csv",
                       header = TRUE)

#### DATA MANIPULATION -----

#### RESIZE FOR DISTORTION ----
df.distort$scale <- 0.8106
df.distort$scale[df.distort$image_id == "6835_KAH_1_box4_img1.tif"] <- 1.01

df.distort$area.um <- df.distort$area/df.distort$scale
df.distort$len.um <- df.distort$majorAxis/df.distort$scale
df.distort$wid.um <- df.distort$minorAxis/df.distort$scale

#### MATCH CLI AND MANUAL ----
## Make subset of data that match what Lily removed
nrow(rm.df)
length(unique(rm.df$image_id)) #38

#remove .jpg from image id
df.cli.int$image_id <- gsub(df.cli.int$image_id,
                            pattern = ".jpg",
                            replacement = "")

rm.df$image_id <- gsub(rm.df$image_id,
                       pattern = ".jpg",
                       replacement = "")

#make sure images overlap
rm.img <- unique(rm.df$image_id)
cli.int.img <- unique(df.cli.int$image_id)

setdiff(cli.int.img, rm.img) 
setdiff(rm.img, cli.int.img) #should be no diff
# differences are because some I have also decided should be done manually
# three are in the omit folder because of excess calcification
# one is actually M. agonistes
diff.img <- setdiff(rm.img, cli.int.img)
rm.df <- rm.df[!(rm.df$image_id %in% diff.img),]
nrow(rm.df)
length(unique(rm.df$image_id)) #17

#make a unique id column
rm.df$id <- paste0(rm.df$image_id, "_", rm.df$box_id)
unique(duplicated(rm.df$id))
rm.df.clean <- rm.df[!duplicated(rm.df$id),]
nrow(rm.df.clean) #425

df.cli.int$id <- paste0(df.cli.int$image_id, "_", df.cli.int$V1)
unique(duplicated(df.cli.int$id))

meta.df$col.id <- paste0(meta.df$Sample_ID, "_", meta.df$Shell_ID, "_", meta.df$Colony_ID)

#make datasets have same images
rm.img.trim <- rm.img[!(rm.img %in% diff.img)]
df.cli.trim <- df.cli.int[df.cli.int$image_id %in% rm.img.trim,]
nrow(df.cli.trim)
length(unique(df.cli.trim$image_id)) #17

##give colony ids to everything
#trim meta to be same images 
meta.df.trim <- meta.df[meta.df$Image_ID %in% rm.img,] #38
meta.df.trim <- meta.df.trim %>%
    select(Image_ID, col.id) %>% 
    as.data.frame()

df.cli.int.meta <- merge(df.cli.trim, meta.df.trim,
                 by.x = "image_id", by.y = "Image_ID")

#make a dataset of the cli with zooids manually removed
df.man.trim <- df.cli.int.meta[!(df.cli.int.meta$id %in% rm.df$id),]
nrow(df.man.trim)

## add in other manual outputs by lily and also remove dupes
df.man.int$image_id <- gsub(df.man.int$image_id,
                            pattern = ".jpg",
                            replacement = "")
df.man.int$id <- paste0(df.man.int$image_id, "_", df.man.int$V1)
## look for dupes
unique(duplicated(df.man.int$id, df.man.trim$id)) #none??

df.man.int.meta <- merge(df.man.int, meta.df.trim,
                         by.x = "image_id", by.y = "Image_ID")

df.man.int.all <- rbind(df.man.trim, df.man.int.meta)

setdiff(df.man.int.all$id, df.cli.int.meta$id) #no diffs

##### MERGE AND MATCH ROTATION ------
## make the rotation df
df.top$treatment <- "top"
df.right$treatment <- "right"
df.bottom$treatment <- "bottom"
df.left$treatment <- "left"

df.rotation <- rbind(df.top, df.right, df.bottom, df.left)

names(df.rotation)[names(df.rotation) == "X"] <- "box_id"

#add in matching box ids
df.rotation$id <- paste0(df.rotation$treatment, "_", df.rotation$box_id)
df.rot.ids$id <- paste0(df.rot.ids$rotation, "_", df.rot.ids$box_id)

df.rot.match <- merge(df.rotation, df.rot.ids,
                      by = "id", 
                      all.x = TRUE, all.y = FALSE)

#### TEST DISTORTION & TILT ----
#going to compare the axes and the area
#std.error <- function(x) sd(x)/sqrt(length(x))
#within = for a category, amount of variation seen between the same zooid but different treatments
#among = for a category, within each colony (i.e. image)
#want to see more variation among (i.e., within a colonies) than between treatment types

distort.img <- c("6835_KAH_1_box4_img2.tif", 
                 "6835_KAH_1_box4_img3.tif",
                 "6835_KAH_1_box4_img4.tif",
                 "6835_KAH_1_box4_img5.tif")

tilt.img <- c("6835_KAH_1_box4_img1.tif",
              "6835_KAH_1_box4_img1a.tif",
              "6835_KAH_1_box4_img2.tif")

distort.within.stats <- df.distort[df.distort$image_id %in% distort.img,] %>%
    group_by(category, code) %>%
    summarise(avg.area = mean(area.um, na.rm = TRUE),
              var.area = var(area.um, na.rm = TRUE),
              avg.len = mean(len.um, na.rm = TRUE),
              var.len = var(len.um, na.rm = TRUE),
              avg.wid = mean(wid.um, na.rm = TRUE),
              var.wid = var(wid.um, na.rm = TRUE),
              n = n()) %>%
    as.data.frame()
distort.within.stats$image_id = ""
distort.within.stats$stat_type = "Within"

distort.among.stats <- df.distort[df.distort$image_id %in% distort.img,] %>%
    group_by(category, image_id) %>%
    summarise(avg.area = mean(area.um, na.rm = TRUE),
              var.area = var(area.um, na.rm = TRUE),
              avg.len = mean(len.um, na.rm = TRUE),
              var.len = var(len.um, na.rm = TRUE),
              avg.wid = mean(wid.um, na.rm = TRUE),
              var.wid = var(wid.um, na.rm = TRUE),
              n = n()) %>%
    as.data.frame()
distort.among.stats$code = ""
distort.among.stats$stat_type = "Among"

distort.stats <- rbind(distort.among.stats, distort.within.stats)
distort.stats$stat_type <- factor(distort.stats$stat_type,
                             levels = c("Within", "Among"))
distort.stats <- tidyr::complete(distort.stats, stat_type, var.area)
distort.stats <- tidyr::complete(distort.stats, stat_type, var.len)
distort.stats <- tidyr::complete(distort.stats, stat_type, var.wid)

tilt.within.stats <- df.distort[df.distort$image_id %in% tilt.img,] %>%
    group_by(category, code) %>%
    summarise(avg.area = mean(area.um, na.rm = TRUE),
              var.area = var(area.um, na.rm = TRUE),
              avg.len = mean(len.um, na.rm = TRUE),
              var.len = var(len.um, na.rm = TRUE),
              avg.wid = mean(wid.um, na.rm = TRUE),
              var.wid = var(wid.um, na.rm = TRUE),
              n = n()) %>%
    as.data.frame()
tilt.within.stats$image_id = ""
tilt.within.stats$stat_type = "Within"

tilt.among.stats <- df.distort[df.distort$image_id %in% tilt.img,] %>%
    group_by(category, image_id) %>%
    summarise(avg.area = mean(area.um, na.rm = TRUE),
              var.area = var(area.um, na.rm = TRUE),
              avg.len = mean(len.um, na.rm = TRUE),
              var.len = var(len.um, na.rm = TRUE),
              avg.wid = mean(wid.um, na.rm = TRUE),
              var.wid = var(wid.um, na.rm = TRUE),
              n = n()) %>%
    as.data.frame()
tilt.among.stats$code = ""
tilt.among.stats$stat_type = "Among"

tilt.stats <- rbind(tilt.among.stats, tilt.within.stats)
tilt.stats$stat_type <- factor(tilt.stats$stat_type,
                               levels = c("Within", "Among"))
tilt.stats <- tidyr::complete(tilt.stats, stat_type, var.area)
tilt.stats <- tidyr::complete(tilt.stats, stat_type, var.len)
tilt.stats <- tidyr::complete(tilt.stats, stat_type, var.wid)

#which ones are the problem
distort.within.stats[which.max(distort.within.stats$var.area),] #MMR, autozooid
distort.within.stats[which.max(distort.within.stats$var.len),] #TMR, autozooid
distort.within.stats[which.max(distort.within.stats$var.wid),] #TMR, autozooid

tilt.within.stats[which.max(tilt.within.stats$var.area),] #UUP, autozooid
tilt.within.stats[which.max(tilt.within.stats$var.len),] #TR, autozooid
tilt.within.stats[which.max(tilt.within.stats$var.wid),] #UUP, autozooid


#UUP and TMR show the most variation
#want to see which img treatment is driving this

df.distort[df.distort$code == "TMR" & df.distort$category == "autozooid",c(1:3, 29:31)] 
#wid and length for distort
#img 3 is largest wid but smallest len; img 5 is smallest wid but largest len

df.distort[df.distort$code == "UUP" & df.distort$category == "autozooid", c(1:3, 29:31)] 
#area and wid for tilt
#img 1 is biggest area and len; img 1a is smallest area and len

###### DISTORTION PLOTS ------
## this is for img 2, 3, 4, 5

# WITHIN (Green)
## COL = #009844
## FILL = #c6e4c7

#AMONG (Pink)
## COL = #e8536b
## FILL = #efdeef

#within, among
stat.col = c("#009844", "#e8536b")
stat.fill = c("#c6e4c7", "#efdeef")

#AUTOZOOID
## AREA
p.dist.auto.area <- distort.stats %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = stat_type, 
           y = log(var.area),
           color = stat_type,
           fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Autozooid~Area~(mu*m^2)),
                       lim = c(5, 25)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.dist.auto.area, 
       file = "./Results/summer_student_project/dist.auto.area.var.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.dist.auto.len <- distort.stats %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.len),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Autozooid~Length~(mu*m)),
                       lim = c(5, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) +
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.dist.auto.len, 
       file = "./Results/summer_student_project/dist.auto.len.var.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.dist.auto.wid <- distort.stats %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.wid),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Autozooid~Width~(mu*m)),
                       lim = c(-5, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.dist.auto.wid, 
       file = "./Results/summer_student_project/dist.auto.wid.var.png", 
       width = 14, height = 10, units = "cm")

#ORIFICE
## AREA
p.dist.ori.area <- distort.stats %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.area),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Orifice~Area~(mu*m^2)),
                       lim = c(5, 20)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.dist.ori.area, 
       file = "./Results/summer_student_project/dist.ori.area.var.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.dist.ori.len <- distort.stats %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.len),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Orifice~Length~(mu*m)),
                       lim = c(-25, 5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) +
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.dist.ori.len, 
       file = "./Results/summer_student_project/dist.ori.len.var.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.dist.ori.wid <- distort.stats %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.wid),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.2)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Orifice~Width~(mu*m)),
                       lim = c(-5, 5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.dist.ori.wid, 
       file = "./Results/summer_student_project/dist.ori.wid.var.png", 
       width = 14, height = 10, units = "cm")

#AVICULARIA

## AREA
p.dist.avic.area <- distort.stats %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.area),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Avicularia~Area~(mu*m^2)),
                       lim = c(5, 15)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.dist.avic.area, 
       file = "./Results/summer_student_project/dist.avic.area.var.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.dist.avic.len <- distort.stats %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.len),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Avicularia~Length~(mu*m)),
                       lim = c(-5, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) +
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.dist.avic.len, 
       file = "./Results/summer_student_project/dist.avic.len.var.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.dist.avic.wid <- distort.stats %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.wid),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.2)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Avicularia~Width~(mu*m)),
                       lim = c(0, 5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.dist.avic.wid, 
       file = "./Results/summer_student_project/dist.avic.wid.var.png", 
       width = 14, height = 10, units = "cm")

##### TILT PLOTS -----
## AREA
p.tilt.auto.area <- tilt.stats %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.area),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Autozooid~Area~(mu*m^2)),
                       lim = c(15, 25)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.tilt.auto.area, 
       file = "./Results/summer_student_project/tilt.auto.area.var.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.tilt.auto.len <- tilt.stats %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.len),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Autozooid~Length~(mu*m)),
                       lim = c(0, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) +
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.tilt.auto.len, 
       file = "./Results/summer_student_project/tilt.auto.len.var.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.tilt.auto.wid <- tilt.stats %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.wid),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Autozooid~Width~(mu*m)),
                       lim = c(0, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.tilt.auto.wid, 
       file = "./Results/summer_student_project/tilt.auto.wid.var.png", 
       width = 14, height = 10, units = "cm")

#ORIFICE
## AREA
p.tilt.ori.area <- tilt.stats %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.area),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Orifice~Area~(mu*m^2)),
                       lim = c(10, 20)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.tilt.ori.area, 
       file = "./Results/summer_student_project/tilt.ori.area.var.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.tilt.ori.len <- tilt.stats %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.len),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Orifice~Length~(mu*m)),
                       lim = c(-5, 5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) +
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.tilt.ori.len, 
       file = "./Results/summer_student_project/tilt.ori.len.var.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.tilt.ori.wid <- tilt.stats %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.wid),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.2)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Orifice~Width~(mu*m)),
                       lim = c(-5, 5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.tilt.ori.wid, 
       file = "./Results/summer_student_project/tilt.ori.wid.var.png", 
       width = 14, height = 10, units = "cm")

#AVICULARIA
## AREA
p.tilt.avic.area <- tilt.stats %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.area),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Avicularia~Area~(mu*m^2)),
                       lim = c(5, 15)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.tilt.avic.area, 
       file = "./Results/summer_student_project/tilt.avic.area.var.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.tilt.avic.len <- tilt.stats %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.len),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Avicularia~Length~(mu*m)),
                       lim = c(-5, 5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) +
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.tilt.avic.len, 
       file = "./Results/summer_student_project/tilt.avic.len.var.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.tilt.avic.wid <- tilt.stats %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = stat_type, 
               y = log(var.wid),
               color = stat_type,
               fill = stat_type)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.2)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Avicularia~Width~(mu*m)),
                       lim = c(-5, 5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.tilt.avic.wid, 
       file = "./Results/summer_student_project/tilt.avic.wid.var.png", 
       width = 14, height = 10, units = "cm")

#### TEST PICKINESS ----
#df.man.int.all
#df.cli.int.meta

df.man.int.all$treatment <- "Manual"
df.cli.int.meta$treatment <- "CLI"

df.int.comp <- rbind(df.cli.int.meta, df.man.int.all)
df.int.comp$treatment <- factor(df.int.comp$treatment,
                                levels = c("Manual", "CLI"))
df.int.comp <- tidyr::complete(df.int.comp, treatment, area)
df.int.comp <- tidyr::complete(df.int.comp, treatment, majorAxis)
df.int.comp <- tidyr::complete(df.int.comp, treatment, minorAxis)

##### DISTRIBUTIONS -----

## AUTOZOOID
##look at all autozooids
#seem to have removed more of the small autozooids

#AREA
p.pick.int.auto.area <- df.int.comp %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = log(area), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Autozooid~Area~(mu*m^2)),
                       lim = c(10, 13)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.auto.area, 
       file = "./Results/summer_student_project/pick.auto.area.png", 
       width = 14, height = 10, units = "cm")

ks.test(log(df.int.comp$area[df.int.comp$treatment == "Manual" 
                             & df.int.comp$category == "autozooid"]), 
        log(df.int.comp$area[df.int.comp$treatment == "CLI"
                             & df.int.comp$category == "autozooid"])) #nonsig

#LENGTH
p.pick.int.auto.len <- df.int.comp %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = log(majorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Autozooid~Length~(mu*m)),
                       lim = c(5, 7)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.auto.len, 
       file = "./Results/summer_student_project/pick.auto.len.png", 
       width = 14, height = 10, units = "cm")

#WIDTH
p.pick.int.auto.wid <- df.int.comp %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = log(minorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Autozooid~Width~(mu*m)),
                       lim = c(5, 6.5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.auto.wid, 
       file = "./Results/summer_student_project/pick.auto.wid.png", 
       width = 14, height = 10, units = "cm")

##ORIFICE
#AREA
p.pick.int.ori.area <- df.int.comp %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = log(area), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Orifice~Area~(mu*m^2)),
                       lim = c(7.5, 10.5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.ori.area, 
       file = "./Results/summer_student_project/pick.ori.area.png", 
       width = 14, height = 10, units = "cm")

ks.test(log(df.int.comp$area[df.int.comp$treatment == "Manual" 
                             & df.int.comp$category == "orifice"]), 
        log(df.int.comp$area[df.int.comp$treatment == "CLI"
                             & df.int.comp$category == "orifice"])) #sig!

#LENGTH
p.pick.int.ori.len <- df.int.comp %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = log(majorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Orifice~Length~(mu*m)),
                       lim = c(4, 5.5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.ori.len, 
       file = "./Results/summer_student_project/pick.ori.len.png", 
       width = 14, height = 10, units = "cm")

#WIDTH
p.pick.int.ori.wid <- df.int.comp %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = log(minorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Orifice~Width~(mu*m)),
                       lim = c(3.5, 5.5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.ori.wid, 
       file = "./Results/summer_student_project/pick.ori.wid.png", 
       width = 14, height = 10, units = "cm")

##AVICULARIA
#AREA
p.pick.int.avic.area <- df.int.comp %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = log(area), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Avicularia~Area~(mu*m^2)),
                       lim = c(7, 11)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.avic.area, 
       file = "./Results/summer_student_project/pick.avic.area.png", 
       width = 14, height = 10, units = "cm")

ks.test(log(df.int.comp$area[df.int.comp$treatment == "Manual" 
                             & df.int.comp$category == "avicularium"]), 
        log(df.int.comp$area[df.int.comp$treatment == "CLI"
                             & df.int.comp$category == "avicularium"])) #nonsig

#LENGTH
p.pick.int.avic.len <- df.int.comp %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = log(majorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Avicularia~Length~(mu*m)),
                       lim = c(3.5, 6)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.avic.len, 
       file = "./Results/summer_student_project/pick.avic.len.png", 
       width = 14, height = 10, units = "cm")

#WIDTH
p.pick.int.avic.wid <- df.int.comp %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = log(minorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Avicularia~Width~(mu*m)),
                       lim = c(3, 5.5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.avic.wid, 
       file = "./Results/summer_student_project/pick.avic.wid.png", 
       width = 14, height = 10, units = "cm")

##OVICELL
#AREA
p.pick.int.ovi.area <- df.int.comp %>%
    filter(category == "ovicell") %>%
    ggplot(aes(x = log(area), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Ovicell~Area~(mu*m^2)),
                       lim = c(8.5, 12)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.ovi.area, 
       file = "./Results/summer_student_project/pick.ovi.area.png", 
       width = 14, height = 10, units = "cm")

ks.test(log(df.int.comp$area[df.int.comp$treatment == "Manual" 
                             & df.int.comp$category == "ovicell"]), 
        log(df.int.comp$area[df.int.comp$treatment == "CLI"
                             & df.int.comp$category == "ovicell"])) #nonsig

#LENGTH
p.pick.int.ovi.len <- df.int.comp %>%
    filter(category == "ovicell") %>%
    ggplot(aes(x = log(majorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Ovicell~Length~(mu*m)),
                       lim = c(4, 6.5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.ovi.len, 
       file = "./Results/summer_student_project/pick.ovi.len.png", 
       width = 14, height = 10, units = "cm")

#WIDTH
p.pick.int.ovi.wid <- df.int.comp %>%
    filter(category == "ovicell") %>%
    ggplot(aes(x = log(minorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.7) +
    plot.theme +
    scale_x_continuous(expression(ln~Ovicell~Width~(mu*m)),
                       lim = c(4, 6.5)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill)

ggsave(p.pick.int.ovi.wid, 
       file = "./Results/summer_student_project/pick.ovi.wid.png", 
       width = 14, height = 10, units = "cm")

##### VARIATION -----
df.int.comp.stats <- df.int.comp %>%
    group_by(treatment, category, col.id) %>%
    summarise(avg.area = mean(area, na.rm = TRUE),
              var.area = var(area, na.rm = TRUE),
              avg.len = mean(majorAxis, na.rm = TRUE),
              var.len = var(majorAxis, na.rm = TRUE),
              avg.wid = mean(minorAxis, na.rm = TRUE),
              var.wid = var(minorAxis, na.rm = TRUE),
              n = n()) %>%
    as.data.frame()

###### VARIATION PLOTS ------
## expect more variation for CLI, but not much more

#AUTOZOOID
## AREA
p.pick.auto.area.var <- df.int.comp.stats %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = treatment, 
               y = log(var.area),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Autozooid~Area~(mu*m^2)),
                       lim = c(15, 25)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.auto.area.var, 
       file = "./Results/summer_student_project/pick.auto.area.var.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.pick.auto.len.var <- df.int.comp.stats %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = treatment, 
               y = log(var.len),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Autozooid~Length~(mu*m)),
                       lim = c(5, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.auto.len.var, 
       file = "./Results/summer_student_project/pick.auto.len.var.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.pick.auto.wid.var <- df.int.comp.stats %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = treatment, 
               y = log(var.wid),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Autozooid~Width~(mu*m)),
                       lim = c(5, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.auto.wid.var, 
       file = "./Results/summer_student_project/pick.auto.wid.var.png", 
       width = 14, height = 10, units = "cm")

#ORIFICE
## AREA
p.pick.ori.area.var <- df.int.comp.stats %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = treatment, 
               y = log(var.area),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Orifice~Area~(mu*m^2)),
                       lim = c(10, 20)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.ori.area.var, 
       file = "./Results/summer_student_project/pick.ori.area.var.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.pick.ori.len.var <- df.int.comp.stats %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = treatment, 
               y = log(var.len),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Orifice~Length~(mu*m)),
                       lim = c(0, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.ori.len.var, 
       file = "./Results/summer_student_project/pick.ori.len.var.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.pick.ori.wid.var <- df.int.comp.stats %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = treatment, 
               y = log(var.wid),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Orifice~Width~(mu*m)),
                       lim = c(0, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.ori.wid.var, 
       file = "./Results/summer_student_project/pick.ori.wid.var.png", 
       width = 14, height = 10, units = "cm")

#AVICULARIA
## AREA
p.pick.avic.area.var <- df.int.comp.stats %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = treatment, 
               y = log(var.area),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Avicularia~Area~(mu*m^2)),
                       lim = c(10, 20)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.avic.area.var, 
       file = "./Results/summer_student_project/pick.avic.area.var.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.pick.avic.len.var <- df.int.comp.stats %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = treatment, 
               y = log(var.len),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Avicularia~Length~(mu*m)),
                       lim = c(0, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.avic.len.var, 
       file = "./Results/summer_student_project/pick.avic.len.var.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.pick.avic.wid.var <- df.int.comp.stats %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = treatment, 
               y = log(var.wid),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Avicularia~Width~(mu*m)),
                       lim = c(0, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.avic.wid.var, 
       file = "./Results/summer_student_project/pick.avic.wid.var.png", 
       width = 14, height = 10, units = "cm")

#OVICELL
## AREA
p.pick.ovi.area.var <- df.int.comp.stats %>%
    filter(category == "ovicell") %>%
    ggplot(aes(x = treatment, 
               y = log(var.area),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Ovicell~Area~(mu*m^2)),
                       lim = c(15, 25)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.ovi.area.var, 
       file = "./Results/summer_student_project/pick.ovi.area.var.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.pick.ovi.len.var <- df.int.comp.stats %>%
    filter(category == "ovicell") %>%
    ggplot(aes(x = treatment, 
               y = log(var.len),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Ovicell~Length~(mu*m)),
                       lim = c(0, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.ovi.len.var, 
       file = "./Results/summer_student_project/pick.ovi.len.var.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.pick.ovi.wid.var <- df.int.comp.stats %>%
    filter(category == "ovicell") %>%
    ggplot(aes(x = treatment, 
               y = log(var.wid),
               color = treatment,
               fill = treatment)) +
    geom_violin() + 
    #stat_summary(fun.data = mean_sdl, mult = 1, 
    #             geom = "pointrange") + 
    geom_jitter(shape = 19, position = position_jitter(0.05)) +
    #geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, shape = 19) + 
    #stat_summary(fun = median, geom = "point", size = 20, shape = "-") +
    plot.theme +
    scale_x_discrete(name = "Variation Comparison") + 
    scale_y_continuous(expression(ln~Variance~of~Ovicell~Width~(mu*m)),
                       lim = c(0, 10)) + 
    scale_color_manual(values = stat.col) + 
    scale_fill_manual(values = stat.fill) + 
    geom_boxplot(width = 0.1, col = "black", fill = "transparent")

ggsave(p.pick.ovi.wid.var, 
       file = "./Results/summer_student_project/pick.ovi.wid.var.png", 
       width = 14, height = 10, units = "cm")

#### TEST ROTATION ----
#df.rot.match; .y means the matching file
df.rot.trim <- df.rot.match[,c(1:4, 7, 12, 13, 28, 30, 31, 32)]
#trim to only those with the same box ids
df.rot.trim <- df.rot.trim[!(is.na(df.rot.trim$box_id.y)),]

## which categories are found or not between treatments?
unique(df.rot.trim$category.y)
mislabel <- c("ovicell (mislabeled)", "orifice (mislabeled)", "avicularium (mislabeled)")
nrow(df.rot.trim[df.rot.trim$category.y %in% mislabel,]) #7 
nrow(df.rot.ids[df.rot.ids$rotation == "top",]) #70 total labels 
#only 7/70 are mislabeled, or 10%
#which do not have ids?
df.rot.ids$cat_id <- paste0(df.rot.ids$category, "_", df.rot.ids$zooid_id)
table(df.rot.ids$cat_id[is.na(df.rot.ids$box_id)]) #anyone with 4 is always blank
#this is 2 of them
length(unique(df.rot.ids$cat_id[is.na(df.rot.ids$box_id)])) #25, but minus 2, so 23 are not always found
#so 25 out of possible 70 are sometimes not found: 35.7% of the time not found

## what are the differences in size estimations?
df.rot.trim$treatment <- factor(df.rot.trim$treatment,
                                levels = c("left", "top",
                                           "right", "bottom"))
df.rot.trim <- tidyr::complete(df.rot.trim, treatment, area)
df.rot.trim <- tidyr::complete(df.rot.trim, treatment, majorAxis)
df.rot.trim <- tidyr::complete(df.rot.trim, treatment, minorAxis)

###### ROTATION PLOTS ------
treat.col = c("#F8766D", "#7CAE00",
              "#00BFC4", "#C77CFF")

#AUTOZOOID
## AREA
p.rot.auto.area <- df.rot.trim %>%
    filter(category.x == "autozooid") %>%
    ggplot(aes(x = log(area), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Autozooid~Area~(mu*m^2)),
                       lim = c(9, 12.5)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.auto.area, 
       file = "./Results/summer_student_project/rot.auto.area.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.rot.auto.len <- df.rot.trim %>%
    filter(category.x == "autozooid") %>%
    ggplot(aes(x = log(majorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Autozooid~Length~(mu*m)),
                       lim = c(5.5, 6.75)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.auto.len, 
       file = "./Results/summer_student_project/rot.auto.len.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.rot.auto.wid <- df.rot.trim %>%
    filter(category.x == "autozooid") %>%
    ggplot(aes(x = log(minorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Autozooid~Width~(mu*m)),
                       lim = c(4.5, 6.5)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.auto.wid, 
       file = "./Results/summer_student_project/rot.auto.wid.png", 
       width = 14, height = 10, units = "cm")

##ORIFICE
## AREA
p.rot.ori.area <- df.rot.trim %>%
    filter(category.x == "orifice") %>%
    ggplot(aes(x = log(area), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Orifice~Area~(mu*m^2)),
                       lim = c(8, 10)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.ori.area, 
       file = "./Results/summer_student_project/rot.ori.area.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.rot.ori.len <- df.rot.trim %>%
    filter(category.x == "orifice") %>%
    ggplot(aes(x = log(majorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Orifice~Length~(mu*m)),
                       lim = c(4.25, 5.25)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.ori.len, 
       file = "./Results/summer_student_project/rot.ori.len.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.rot.ori.wid <- df.rot.trim %>%
    filter(category.x == "orifice") %>%
    ggplot(aes(x = log(minorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Orifice~Width~(mu*m)),
                       lim = c(3.5, 5.5)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.ori.wid, 
       file = "./Results/summer_student_project/rot.ori.wid.png", 
       width = 14, height = 10, units = "cm")

##AVICULARIA
## AREA
p.rot.avic.area <- df.rot.trim %>%
    filter(category.x == "avicularium") %>%
    ggplot(aes(x = log(area), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Avicularia~Area~(mu*m^2)),
                       lim = c(7, 9.75)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.avic.area, 
       file = "./Results/summer_student_project/rot.avic.area.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.rot.avic.len <- df.rot.trim %>%
    filter(category.x == "avicularium") %>%
    ggplot(aes(x = log(majorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Avicularia~Length~(mu*m)),
                       lim = c(3.75, 5.5)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.avic.len, 
       file = "./Results/summer_student_project/rot.avic.len.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.rot.avic.wid <- df.rot.trim %>%
    filter(category.x == "avicularium") %>%
    ggplot(aes(x = log(minorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Avicularia~Width~(mu*m)),
                       lim = c(3, 5)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.avic.wid, 
       file = "./Results/summer_student_project/rot.avic.wid.png", 
       width = 14, height = 10, units = "cm")

##OVICELL
## AREA
p.rot.ovi.area <- df.rot.trim %>%
    filter(category.x == "ovicell") %>%
    ggplot(aes(x = log(area), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Ovicell~Area~(mu*m^2)),
                       lim = c(8.5, 12)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.ovi.area, 
       file = "./Results/summer_student_project/rot.ovi.area.png", 
       width = 14, height = 10, units = "cm")

## LENGTH
p.rot.ovi.len <- df.rot.trim %>%
    filter(category.x == "ovicell") %>%
    ggplot(aes(x = log(majorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Ovicell~Length~(mu*m)),
                       lim = c(4.5, 6.5)) +
    scale_color_manual(values = treat.col) + 
    scale_fill_manual(values = treat.col)

ggsave(p.rot.ovi.len, 
       file = "./Results/summer_student_project/rot.ovi.len.png", 
       width = 14, height = 10, units = "cm")

## WIDTH
p.rot.ovi.wid <- df.rot.trim %>%
    filter(category.x == "ovicell") %>%
    ggplot(aes(x = log(minorAxis), 
               group = treatment,
               color = treatment,
               fill = treatment)) +
    #geom_histogram(bins = 15) + 
    geom_density(alpha = 0.5) +
    plot.theme +
    scale_x_continuous(expression(ln~Ovicell~Width~(mu*m)),
                       lim = c(4.5, 6.25))

ggsave(p.rot.ovi.wid, 
       file = "./Results/summer_student_project/rot.ovi.wid.png", 
       width = 14, height = 10, units = "cm")

#### TEST OUTLIER DETECTION ----
## using intermedia as an example
#df.cli.int
#remove edm because at different magnicifaction
unique(df.cli.int$image_id)
rm.edm <- c("edm8146", "edm8147", "edm8148", "edm8160", "edm8170",
            "edm8173", "edm8261", "edm8329", "edm8330", "edm8331",
            "edm8332", "edm8459", "edm8460", "edm8510", "edm8528",
            "edm8529", "edm8530", "edm8535", "edm8539", "edm8542",
            "edm8543", "edm8551", "edm8552", "edm8580", "edm8581", 
            "edm8584", "edm8640", "edm8641", "edm8642", "edm8643",
            "edm8646", "edm8647", "edm8648", "edm9268")
df.cli.int.trim <- df.cli.int[!(df.cli.int$image_id %in% rm.edm),]


#it is failing to catch the small autozooids
df.cli.int.trim %>%
    filter(category == "autozooid") %>%
    ggplot(aes(x = log(area))) +
    #geom_histogram(bins = 15) + 
    geom_histogram(bins = 30) +
    plot.theme +
    scale_x_continuous(expression(ln~Autozooid~Area~(mu*m^2)))

df.cli.int.trim %>%
    filter(category == "orifice") %>%
    ggplot(aes(x = log(area))) +
    #geom_histogram(bins = 15) + 
    geom_histogram(bins = 30) +
    plot.theme +
    scale_x_continuous(expression(ln~Autozooid~Area~(mu*m^2)))

df.cli.int.trim %>%
    filter(category == "avicularium") %>%
    ggplot(aes(x = log(area))) +
    #geom_histogram(bins = 15) + 
    geom_histogram(bins = 30) +
    plot.theme +
    scale_x_continuous(expression(ln~Autozooid~Area~(mu*m^2)))

df.cli.int.trim %>%
    filter(category == "ovicell") %>%
    ggplot(aes(x = log(area))) +
    #geom_histogram(bins = 15) + 
    geom_histogram(bins = 30) +
    plot.theme +
    scale_x_continuous(expression(ln~Autozooid~Area~(mu*m^2)))
#ovicell is wild

#because of long left tail, maybe make Tmin stricter

#log transform
df.cli.int.trim$ln.area <- log(df.cli.int.trim$area)
df.cli.int.trim$ln.len <- log(df.cli.int.trim$majorAxis)
df.cli.int.trim$ln.wid <- log(df.cli.int.trim$minorAxis)

## test will be values more than 3 sd away from mean
# from https://www.reneshbedre.com/blog/find-outliers.html
# get mean and Standard deviation
mean.auto.area = mean(df.cli.int.trim$ln.area[df.cli.int.trim$category == "autozooid"])
std.auto.area = sd(df.cli.int.trim$ln.area[df.cli.int.trim$category == "autozooid"])

mean.avic.area = mean(df.cli.int.trim$ln.area[df.cli.int.trim$category == "avicualrium"])
std.avic.area = sd(df.cli.int.trim$ln.area[df.cli.int.trim$category == "avicualrium"])

mean.ori.area = mean(df.cli.int.trim$ln.area[df.cli.int.trim$category == "orifice"])
std.ori.area = sd(df.cli.int.trim$ln.area[df.cli.int.trim$category == "orifice"])

mean.ovi.area = mean(df.cli.int.trim$ln.area[df.cli.int.trim$category == "ovicell"])
std.ovi.area = sd(df.cli.int.trim$ln.area[df.cli.int.trim$category == "ovicell"])

# get threshold values for outliers
Tmin.auto.area = mean.auto.area - (2.5*std.auto.area)
Tmax.auto.area = mean.auto.area + (3*std.auto.area)

Tmin.avic.area = mean.avic.area - (2.5*std.avic.area)
Tmax.avic.area = mean.avic.area + (3*std.avic.area)

Tmin.ori.area = mean.ori.area - (2.5*std.ori.area)
Tmax.ori.area = mean.ori.area + (3*std.ori.area)

Tmin.ovi.area = mean.ovi.area - (2.5*std.ovi.area)
Tmax.ovi.area = mean.ovi.area + (3*std.ovi.area)

# find outliers
df.cli.int.trim$mean.area <- ""
df.cli.int.trim$Tmin <- ""
df.cli.int.trim$Tmax <- ""
df.cli.int.trim$outlier <- "N"

df.cli.int.trim$mean.area[df.cli.int.trim$category == "autozooid"] <- mean.auto.area
df.cli.int.trim$mean.area[df.cli.int.trim$category == "avicularium"] <- mean.avic.area
df.cli.int.trim$mean.area[df.cli.int.trim$category == "orifice"] <- mean.ori.area
df.cli.int.trim$mean.area[df.cli.int.trim$category == "ovicell"] <- mean.ovi.area

df.cli.int.trim$Tmin[df.cli.int.trim$category == "autozooid"] <- Tmin.auto.area
df.cli.int.trim$Tmin[df.cli.int.trim$category == "avicularium"] <- Tmin.avic.area
df.cli.int.trim$Tmin[df.cli.int.trim$category == "orifice"] <- Tmin.ori.area
df.cli.int.trim$Tmin[df.cli.int.trim$category == "ovicell"] <- Tmin.ovi.area

df.cli.int.trim$Tmax[df.cli.int.trim$category == "autozooid"] <- Tmax.auto.area
df.cli.int.trim$Tmax[df.cli.int.trim$category == "avicularium"] <- Tmax.avic.area
df.cli.int.trim$Tmax[df.cli.int.trim$category == "orifice"] <- Tmax.ori.area
df.cli.int.trim$Tmax[df.cli.int.trim$category == "ovicell"] <- Tmax.ovi.area

df.cli.int.trim$outlier[df.cli.int.trim$ln.area > df.cli.int.trim$Tmax | df.cli.int.trim$ln.area < df.cli.int.trim$Tmin] <- "Y"

df.cli.int.trim$outlier[df.cli.int.trim$category == "ascopore"] <- ""

unique(df.cli.int.trim$image_id[df.cli.int.trim$outlier == "Y"])
#let's look at some of these
df.cli.int.trim$V1[df.cli.int.trim$outlier == "Y" & df.cli.int.trim$image_id == "MHR.12566"]
#look at this in DeepBryo, I expect the following to be removed: 20, 18, 11, 21, 17, 16, 54
#only overlapping one is 54....

df.cli.int.trim$V1[df.cli.int.trim$outlier == "Y" & df.cli.int.trim$image_id == "MHR.7222"]
#looking at DeepBryo, I expect the following to be removed: 10, 13, 11, 25, 49, 14
#only 25 overlaps...

df.cli.int.trim$V1[df.cli.int.trim$outlier == "Y" & df.cli.int.trim$image_id == "MHR.4724"]
#looking at DeepBryo, I expect the following to be removed: 16, 45, 37
#two overlaps (45, 37)

##Mahalanobis Outlier test----
auto.sub <- df.cli.int.trim %>%
    filter(category == "autozooid") %>%
    select(ln.area)
auto.index <- maha(auto.sub, cutoff = 0.9, rnames = FALSE)[[2]] #index of outliers

xx <- auto.sub$ln.area[auto.index]

df.cli.int.trim$outlier.maha <- ""
df.cli.int.trim$outlier.maha[df.cli.int.trim$ln.area %in% xx 
                            & df.cli.int.trim$category == "autozooid"] <- "Y"

nrow(df.cli.int.trim[df.cli.int.trim$outlier.maha == "Y",]) #157
length(xx) #157 same length! 

#let's see which these are!
unique(df.cli.int.trim$image_id[df.cli.int.trim$outlier.maha == "Y"])

df.cli.int.trim$V1[df.cli.int.trim$outlier.maha == "Y" & df.cli.int.trim$image_id == "MHR.12566"]
#just 20, that is one which I thought should be...

## seems to be a lot of the scale bar left ones...

#### TEST CIRCULARITY ----
df.cli.int.trim$circularity
#closer to 1 is more like a circle

df.cli.int.trim$image_id[df.cli.int.trim$circularity > 0.9]

df.cli.int.trim$V1[df.cli.int.trim$circularity > 0.9
                   & df.cli.int.trim$image_id == "MHR.7995"]
#58, which is a big hole, but misses 64 and 61


#### TEST POLYGON ----

