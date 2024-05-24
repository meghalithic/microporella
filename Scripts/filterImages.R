#This script combines filtering steps from microporella_imageMetadta and 
#microporella_metadata for a list of images that we will use for ML.

#### ENVIRONMENT ----
source("Scripts/env.R")

#### LOAD DATA ---
meta.image <- read.csv("Data/image.metadata.filtered.csv",
                       header = TRUE)

image.df <- read.csv("Data/image.imageMetadata.filtered.csv",
                     header = TRUE)

#### COMBINE ----
head(meta.image$Image_ID)
head(image.df$Image_ID)

nrow(meta.image) #7262
nrow(image.df) #7324

length(setdiff(meta.image$Image_ID, image.df$Image_ID)) #13
length(setdiff(image.df$Image_ID, meta.image$Image_ID)) #72
#going to lose 85 images (total 192 from all other filtering scripts)

meta.image.trim <- meta.image[meta.image$Image_ID %in% image.df$Image_ID,]
nrow(meta.image.trim) #7249
image.trim <- image.df[image.df$Image_ID %in% meta.image$Image_ID,]
nrow(image.trim) #7249

filter.df <- merge(image.trim, meta.image.trim,
                   by = "Image_ID")
nrow(filter.df) #7249

#### TALLY ----
filter.unique <- filter.df[!duplicated(filter.df$ID),]
nrow(filter.unique) #2038

#how many of each species
table(filter.unique$binomial)

#how many colonies per species per formation
table(filter.unique$Formation, filter.unique$binomial)

#### WRITE CSV ----
write.csv(filter.df,
          "Data/image.filter.csv",
          row.names = FALSE)


