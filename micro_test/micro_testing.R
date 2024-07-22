require(dplyr)
require(ggplot2)

df <- read.csv(file = "6835_KAH_1_box4_distortion_comparisons.csv",
               header = TRUE)

colnames(df)
str(df)

#resize
df$scale <- 0.8106
df$scale[df$image_id == "6835_KAH_1_box4_img1.tif"] <- 1.01

df$area.um <- df$area/df$scale
df$len.um <- df$majorAxis/df$scale
df$wid.um <- df$minorAxis/df$scale

#going to compare the axes and the area
#std.error <- function(x) sd(x)/sqrt(length(x))

df.within.stats <- df %>%
  group_by(category, code) %>%
  summarise(avg.area = mean(area.um, na.rm = TRUE),
            var.area = var(area.um), na.rm = TRUE,
            avg.len = mean(len.um, na.rm = TRUE),
            var.len = var(len.um, na.rm = TRUE),
            avg.wid = mean(wid.um, na.rm = TRUE),
            var.wid = var(wid.um, na.rm = TRUE),
            n = n()) %>%
  as.data.frame()

df.among.stats <- df %>%
  group_by(category, image_id) %>%
  summarise(avg.area = mean(area.um, na.rm = TRUE),
            var.area = var(area.um, na.rm = TRUE),
            avg.len = mean(len.um, na.rm = TRUE),
            var.len = var(len.um, na.rm = TRUE),
            avg.wid = mean(wid.um, na.rm = TRUE),
            var.wid = var(wid.um, na.rm = TRUE),
            n = n()) %>%
  as.data.frame()

ggplot() +
  geom_density(aes(df.within.stats$var.area[df.within.stats$category == "autozooid"]),
                 col = "green") +
  geom_density(aes(df.among.stats$var.area[df.among.stats$category == "autozooid"]),
                 col = "pink")

ggplot() +
  geom_density(aes(df.within.stats$var.len[df.within.stats$category == "autozooid"]),
               col = "green") +
  geom_density(aes(df.among.stats$var.len[df.among.stats$category == "autozooid"]),
               col = "pink")

ggplot() +
  geom_density(aes(df.within.stats$var.wid[df.within.stats$category == "autozooid"]),
               col = "green") +
  geom_density(aes(df.among.stats$var.wid[df.among.stats$category == "autozooid"]),
               col = "pink")

#which ones are the problem
which.max(df.within.stats$var.area)
which.max(df.within.stats$var.len)
which.max(df.within.stats$var.wid)

df.within.stats[11,] #AL autozooid for area and len
df.within.stats[17,] #TMR autozooid for wid

#TR and TL show the most variation
#want to see which imge treatment is driving this
df[df$code == "AL" & df$category == "autozooid",] #img 2 is smallest for area, img 1 is largest for area and len; img 3 smallest for len
#image one is head on

df[df$code == "TMR" & df$category == "autozooid",] #img 3 is largest and 5 is smallest