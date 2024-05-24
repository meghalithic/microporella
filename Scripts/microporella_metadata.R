#This script reconciles image names with metadata to ensure that there are no errors
#1. matches images with associated metadata
#2. checks for discrepancies in info

#### LOAD PACKAGES ----
source("Scripts/env.R")

#### LOAD DATA ----
meta.df <- read.csv("Data/Microporella_SEMs_EDM+Mali_20.09.2022.csv",
                    header = TRUE)

images.path <- "../../../../../voje-lab/Desktop/microporella-jpg"

imageNames.jpg = list.files(path = images.path,
                        full.names = TRUE,
                        recursive = TRUE)
imageNames.jpg.trim <- gsub(imageNames.jpg,
                        pattern = paste0(images.path, "/"),
                        replacement = "")
imageNames <- gsub(imageNames.jpg.trim,
                        pattern = ".jpg",
                        replacement = "")

#### COMBINE ----
length(imageNames) #7333
imageNames[duplicated(imageNames)] #none

nrow(meta.df) #8828
dupes <-meta.df$Image_ID[duplicated(meta.df$Image_ID)] #these are repeats
######for now, ignore and remove these-------
meta.df.trim <- meta.df[!(meta.df$Image_ID %in% dupes),]
nrow(meta.df.trim) #8812

#setdiff(imageNames, meta.df$Image_ID) #these are not in the metadata files...
#length(setdiff(imageNames, meta.df$Image_ID)) #63
setdiff(imageNames, meta.df.trim$Image_ID) #these are not in the metadata files...
length(setdiff(imageNames, meta.df.trim$Image_ID)) #71

meta.image <- meta.df.trim[meta.df.trim$Image_ID %in% imageNames,]
nrow(meta.image) #7262 
meta.image$Image_ID[duplicated(meta.image$Image_ID)] #none

#### ABOU DATA ----
#binomials
meta.image$binomial <- paste0(meta.image$Genus, "_", meta.image$Species)

#unique colonies
meta.image$ID <- paste0(meta.image$Sample_ID, "_", meta.image$Shell_ID, 
                        "_", meta.image$Colony_ID)

##### FIX ERRORS -----
unique(meta.image$binomial)
meta.image$binomial[meta.image$binomial == "MIcroporella_speculum"] <- "Microporella_speculum"
meta.image$binomial[meta.image$binomial == "MIcroporella_agonistes"] <- "Microporella_agonistes"

unique(meta.image$Formation)
meta.image$Formation[meta.image$Formation == "TAINUI SB"] <- "Tainui Shellbed"
meta.image$Formation[meta.image$Formation == "Nukumaru Brown Sand"] <- "NKBS"
meta.image$Formation[meta.image$Formation == "Nukumaru Limestone"] <- "NKLS"
meta.image$Formation[meta.image$Formation == "TEWKESBURY"] <- "Tewkesbury"
#waipuru is part of Tewkesbury
meta.image$Formation[meta.image$Formation == "Waipuru Shellbed"] <- "Tewkesbury"

##### TALLY -----
meta.image.unique <- meta.image[!duplicated(meta.image$ID),]
nrow(meta.image.unique) #2044

#how many of each species
table(meta.image.unique$binomial)

#how many colonies per species per formation
table(meta.image.unique$Formation, meta.image.unique$binomial)

#### WRITE OUT DATA ----
write.csv(meta.image,
          "Data/image.metadata.filtered.csv",
          row.names = FALSE)
#lost 71 images
