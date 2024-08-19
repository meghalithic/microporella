##1. combine files
##2. match to metadata
###2a. match to vouchers, micro, bleed
###2b. match to the final dataset used by EDM
##3. add appropriate scale

#### ENVIRONMENT ----
require(dplyr)
require(ggplot2)
require(purrr)
require(data.table)
require(Hmisc)
library(scales)

plot.theme <- theme(text = element_text(size = 16),
                    legend.position = "none",
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    plot.background = element_rect(fill = 'transparent', color = NA))

#### DATA ----
mag.df <- read.csv("Data/non-50-magnification-metadata_18Jul.csv",
                   header = TRUE)

edm.df <- read.csv("Data/Microporella_SELECT_final_datas.csv",
                   header = TRUE)

##### METADATA ----
meta.df <- read.csv("Data/Microporella_SEMs_EDM+Mali_05.06.2024.csv",
                    header = TRUE)

#not using any voucher images
#vouchers <- read.csv("Data/Vouchers.csv",
#                     header = TRUE)

#not using any bleed images
#bleed <- read.csv("Data/BLEED_DB.csv",
#                  header = TRUE)

form.df <- read.csv("Data/Formation_Age in Ma_L&R2005_EDM_SELECT_final_datasets.csv",
                    header = TRUE)

##### M. AGONISTES -----
agon.list = list.files(path = "Data/CLI-outputs/M.agonistes/",
                      pattern = "\\.csv$",
                      full.names = TRUE,
                      recursive = TRUE)

agon.df <- do.call(rbind, lapply(agon.list, function(x) fread(x, stringsAsFactors = FALSE)))
nrow(agon.df)
length(unique(agon.df$image_id)) #126

##### M. DISCORS -----
disc.list = list.files(path = "Data/CLI-outputs/M.discors/",
                      pattern = "\\.csv$",
                      full.names = TRUE,
                      recursive = TRUE)

disc.df <- do.call(rbind, lapply(disc.list, function(x) fread(x, stringsAsFactors = FALSE)))
nrow(disc.df)
length(unique(disc.df$image_id)) #332

##### M. INTERMEDIA -----
int.list = list.files(path = "Data/CLI-outputs/M.intermedia/",
                      pattern = "\\.csv$",
                      full.names = TRUE,
                      recursive = TRUE)

int.df <- do.call(rbind, lapply(int.list, function(x) fread(x, stringsAsFactors = FALSE)))
nrow(int.df)
length(unique(int.df$image_id)) #126

##### M. SPECULUM -----
spec.list = list.files(path = "Data/CLI-outputs/M.speculum/",
                      pattern = "\\.csv$",
                      full.names = TRUE,
                      recursive = TRUE)

spec.df <- do.call(rbind, lapply(spec.list, function(x) fread(x, stringsAsFactors = FALSE)))
nrow(spec.df)
length(unique(spec.df$image_id)) #126

#### MANIPULATION ----
colnames(meta.df) #match on image_ID
olnames(edm.df)

## get rid of ".jpg"

agon.df$image_id <- gsub(".jpg", "",
                         agon.df$image_id)

disc.df$image_id <- gsub(".jpg", "",
                         disc.df$image_id)

int.df$image_id <- gsub(".jpg", "",
                        int.df$image_id)

spec.df$image_id <- gsub(".jpg", "",
                         spec.df$image_id)

##### SET SCALE -----
#scale for 50 mag is: 1.01
agon.df$scale <- 1.01
disc.df$scale <- 1.01
int.df$scale <- 1.01
spec.df$scale <- 1.01

#40 mag is 0.808
#60 mag is 1.208
#30 mag is 0.605

colnames(mag.df)
head(mag.df$Filename)

#see if have processed any that are not x50
## NOTE: this may change and continue to process
intersect(agon.df$image_id, mag.df$Filename)
intersect(disc.df$image_id, mag.df$Filename) #MHR.8542
intersect(int.df$image_id, mag.df$Filename)
intersect(spec.df$image_id, mag.df$Filename)

mag.df$Magnification[mag.df$Filename == "MHR.8542"] #60
disc.df$scale[disc.df$image_id == "MHR.8542"] <- 1.208

##### MAKE UNIQUE IDS -----
#colony id based off sample, shell, and colony number
meta.df$col.id <- paste0(meta.df$Sample_ID, "_", meta.df$Shell_ID, "_", meta.df$Colony_ID)

agon.df$id <- paste0(agon.df$image_id, "_", agon.df$V1)
unique(duplicated(agon.df$id)) #should be FALSE

disc.df$id <- paste0(disc.df$image_id, "_", disc.df$V1)
unique(duplicated(disc.df$id))

int.df$id <- paste0(int.df$image_id, "_", int.df$V1)
unique(duplicated(int.df$id))

spec.df$id <- paste0(spec.df$image_id, "_", spec.df$V1)
unique(duplicated(spec.df$id))

##give colony ids to everything
agon.meta <- merge(agon.df, meta.df,
                   by.x = "image_id", by.y = "Image_ID")
disc.meta <- merge(disc.df, meta.df,
                   by.x = "image_id", by.y = "Image_ID")
int.meta <- merge(int.df, meta.df,
                  by.x = "image_id", by.y = "Image_ID")
spec.meta <- merge(spec.df, meta.df,
                   by.x = "image_id", by.y = "Image_ID")

## check that names match the dataset
unique(agon.meta$Species) #which ones don't match?
unique(agon.meta$image_id[agon.meta$Species != "agonistes"])
# "edm10004" "edm10013" "edm10014" "edm10015" "edm10016" "edm10021" "edm10022" "edm10023" "edm10025"
unique(disc.meta$Species)
unique(int.meta$Species)
unique(spec.meta$Species)


##### MATCH TO EDM DATASET ------
## NOTE: this may change as process more images
colnames(edm.df)
## look for what's I have that EDM doesn't and check to see why

setdiff(agon.meta$image_id, edm.df$Image_ID) 
#edm7774, edm7775
#these are BLEED636, M.agonistes, maybe she didn't use bleed data?
setdiff(disc.meta$image_id, edm.df$Image_ID)
setdiff(int.meta$image_id, edm.df$Image_ID)
setdiff(spec.meta$image_id, edm.df$Image_ID)

##### ADD FORMATION INFO -----
colnames(form.df)
head(form.df$ID)
