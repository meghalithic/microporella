#This script combines filtering steps from microporella_imageMetadta and 
#microporella_metadata for a list of images that we will use for ML.

#went through images manually
#hese filter files are to make sure that no images are included that shouldnt be
#and also that we have all the metadata for the images

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

nrow(meta.image) #6593
nrow(image.df) #7360

length(setdiff(meta.image$Image_ID, image.df$Image_ID)) #17; 0 after I fix names
length(setdiff(image.df$Image_ID, meta.image$Image_ID)) #lots, likely because I have a lot of images of non-focal species0000

image.df$Image_ID <- gsub("[(]x50[)]", 
                          "",
                          image.df$Image_ID)

meta.image.trim <- meta.image[meta.image$Image_ID %in% image.df$Image_ID,]
nrow(meta.image.trim) #6593
image.trim <- image.df[image.df$Image_ID %in% meta.image$Image_ID,]
nrow(image.trim) #6593

filter.df <- merge(image.trim, meta.image.trim,
                   by = "Image_ID")
nrow(filter.df) #6593

#### TALLY ----
filter.unique <- filter.df[!duplicated(filter.df$ID),]
nrow(filter.unique) #1876

#how many of each species
table(filter.unique$binomial)

#how many colonies per species per formation
table(filter.unique$Formation, filter.unique$binomial)

#### WRITE CSV ----
write.csv(filter.df,
          "Data/image.filter.csv",
          row.names = FALSE)

#### WRITE OUT LIST OF IMAGES NOT AT x50 ----
non50 <- filter.df[filter.df$magCheck == FALSE,]
non50$Mag
write.csv(non50,
          "Data/image.filter.non50mag.csv",
          row.names = FALSE)

