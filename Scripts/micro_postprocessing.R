
## be sure to use list of images not at 50 to set the scale for all images

#### SET UP ENV ---
require(purrr)
require(data.table)
require(dplyr)
require(ggplot2)

#### LOAD DATA ----
csv.list = list.files(path = "Data/CLI-outputs/M.intermedia/",
                      pattern = "\\.csv$",
                      full.names = TRUE,
                      recursive = TRUE)

df <- do.call(rbind, lapply(csv.list, function(x) fread(x, stringsAsFactors = FALSE)))
nrow(df)
length(unique(df$image_id)) #185

rm.df <- read.csv("Data/Manual-outputs/M.intermedia/image_id_with_object_ids_to_remove.csv",
                  header = TRUE)

meta.df <- read.csv("Data/Microporella_SEMs_EDM+Mali_05.06.2024.csv",
                    header = TRUE)

#### DATA MANIPULATION ---
nrow(rm.df)
length(unique(rm.df$image_id)) #38

#remove .jpg from image id
df$image_id <- gsub(df$image_id,
                    pattern = ".jpg",
                    replacement = "")

rm.df$image_id <- gsub(rm.df$image_id,
                       pattern = ".jpg",
                       replacement = "")

#make sure images overlap
rm.img <- unique(rm.df$image_id)
img <- unique(df$image_id)

setdiff(img, rm.img) 
setdiff(rm.img, img) #should be no diff

#make a unique id column
rm.df$id <- paste0(rm.df$image_id, "_", rm.df$box_id)
duplicated(rm.df$id)
rm.df.clean <- rm.df[!duplicated(rm.df$id),]
nrow(rm.df.clean) #1003

df$id <- paste0(df$image_id, "_", df$V1)
unique(duplicated(df$id))

meta.df$col.id <- paste0(meta.df$Sample_ID, "_", meta.df$Shell_ID, "_", meta.df$Colony_ID)

#make datasets have same images
df.cli.trim <- df[df$image_id %in% rm.img,]
nrow(df.cli.trim)
length(unique(df.cli.trim$image_id)) #38

##give colony ids to everything
#trim meta to be same images 
meta.df.trim <- meta.df[meta.df$Image_ID %in% rm.img,] #38
meta.df.trim <- meta.df.trim %>%
  select(Image_ID, col.id) %>% 
  as.data.frame()

df.meta <- merge(df.cli.trim, meta.df.trim,
                 by.x = "image_id", by.y = "Image_ID")

#make a dataset of the cli with zooids manually removed
df.cli <- df.meta
df.man <- df.cli[!(df.cli$id %in% rm.df$id),]
nrow(df.man)

setdiff(df.man$id, df.cli$id)

#### TEST FOR DIFFERENCES ----
##see how different they are

##look at all autozooids
#seem to have removed more of the small autozooids
ggplot() +
  geom_density(aes(df.cli$area[df.cli$category == "autozooid"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$area[df.man$category == "autozooid"]),
               col = "pink")

ggplot() +
  geom_density(aes(df.cli$majorAxis[df.cli$category == "autozooid"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$majorAxis[df.man$category == "autozooid"]),
               col = "pink")

ggplot() +
  geom_density(aes(df.cli$minorAxis[df.cli$category == "autozooid"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$minorAxis[df.man$category == "autozooid"]),
               col = "pink")

##look at all avicularia
#seem to have removed more of the larger ones
ggplot() +
  geom_density(aes(df.cli$area[df.cli$category == "avicularium"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$area[df.man$category == "avicularium"]),
               col = "pink")

ggplot() +
  geom_density(aes(df.cli$majorAxis[df.cli$category == "avicularium"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$majorAxis[df.man$category == "avicularium"]),
               col = "pink")

ggplot() +
  geom_density(aes(df.cli$minorAxis[df.cli$category == "avicularium"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$minorAxis[df.man$category == "avicularium"]),
               col = "pink")

##look at all orifices
#seem to have removed more of the larger ones
ggplot() +
  geom_density(aes(df.cli$area[df.cli$category == "orifice"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$area[df.man$category == "orifice"]),
               col = "pink")

ggplot() +
  geom_density(aes(df.cli$majorAxis[df.cli$category == "orifice"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$majorAxis[df.man$category == "orifice"]),
               col = "pink")

ggplot() +
  geom_density(aes(df.cli$minorAxis[df.cli$category == "orifice"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$minorAxis[df.man$category == "orifice"]),
               col = "pink")

##look at all ovicells
ggplot() +
  geom_density(aes(df.cli$area[df.cli$category == "ovicell"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$area[df.man$category == "ovicell"]),
               col = "pink")

ggplot() +
  geom_density(aes(df.cli$majorAxis[df.cli$category == "ovicell"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$majorAxis[df.man$category == "ovicell"]),
               col = "pink")

ggplot() +
  geom_density(aes(df.cli$minorAxis[df.cli$category == "ovicell"]), #not a big diff
               col = "green") +
  geom_density(aes(df.man$minorAxis[df.man$category == "ovicell"]),
               col = "pink")


df.cli.stats <- df.cli %>%
  group_by(category, col.id) %>%
  summarise(avg.area = mean(area, na.rm = TRUE),
            var.area = var(area, na.rm = TRUE),
            avg.len = mean(majorAxis, na.rm = TRUE),
            var.len = var(majorAxis, na.rm = TRUE),
            avg.wid = mean(minorAxis, na.rm = TRUE),
            var.wid = var(minorAxis, na.rm = TRUE),
            n = n()) %>%
  as.data.frame()

df.man.stats <- df.man %>%
  group_by(category, col.id) %>%
  summarise(avg.area = mean(area, na.rm = TRUE),
            var.area = var(area, na.rm = TRUE),
            avg.len = mean(majorAxis, na.rm = TRUE),
            var.len = var(majorAxis, na.rm = TRUE),
            avg.wid = mean(minorAxis, na.rm = TRUE),
            var.wid = var(minorAxis, na.rm = TRUE),
            n = n()) %>%
  as.data.frame()

#variation within colony 
##autozooid
ggplot() +
  geom_density(aes(df.cli.stats$var.area[df.cli.stats$category == "autozooid"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.area[df.man.stats$category == "autozooid"]),
               col = "pink")
#reducing within colony variation by a lot

ggplot() +
  geom_density(aes(df.cli.stats$var.len[df.cli.stats$category == "autozooid"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.len[df.man.stats$category == "autozooid"]),
               col = "pink")
#not a big different in length

ggplot() +
  geom_density(aes(df.cli.stats$var.wid[df.cli.stats$category == "autozooid"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.wid[df.man.stats$category == "autozooid"]),
               col = "pink")
#remove a lot of variation in width

##avicularia
ggplot() +
  geom_density(aes(df.cli.stats$var.area[df.cli.stats$category == "avicularium"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.area[df.man.stats$category == "avicularium"]),
               col = "pink")
#not too big of a diff

ggplot() +
  geom_density(aes(df.cli.stats$var.len[df.cli.stats$category == "avicularium"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.len[df.man.stats$category == "avicularium"]),
               col = "pink")
#not a big different in length

ggplot() +
  geom_density(aes(df.cli.stats$var.wid[df.cli.stats$category == "avicularium"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.wid[df.man.stats$category == "avicularium"]),
               col = "pink")
#remove a lot of variation in width

##orifice
ggplot() +
  geom_density(aes(df.cli.stats$var.area[df.cli.stats$category == "orifice"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.area[df.man.stats$category == "orifice"]),
               col = "pink")
#not too big of a diff

ggplot() +
  geom_density(aes(df.cli.stats$var.len[df.cli.stats$category == "orifice"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.len[df.man.stats$category == "orifice"]),
               col = "pink")
#remove a lot of var in len

ggplot() +
  geom_density(aes(df.cli.stats$var.wid[df.cli.stats$category == "orifice"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.wid[df.man.stats$category == "orifice"]),
               col = "pink")
#remove a lot of variation in width

##ovicell
ggplot() +
  geom_density(aes(df.cli.stats$var.area[df.cli.stats$category == "ovicell"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.area[df.man.stats$category == "ovicell"]),
               col = "pink")
#not too big of a diff

ggplot() +
  geom_density(aes(df.cli.stats$var.len[df.cli.stats$category == "ovicell"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.len[df.man.stats$category == "ovicell"]),
               col = "pink")
#not a big diff

ggplot() +
  geom_density(aes(df.cli.stats$var.wid[df.cli.stats$category == "ovicell"]),
               col = "green") +
  geom_density(aes(df.man.stats$var.wid[df.man.stats$category == "ovicell"]),
               col = "pink")
#not a big diff