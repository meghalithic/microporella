require(tidyr)

edm.df <- read.csv("../../../../Desktop/edm_select_data.csv",
                   header = TRUE)

edm.meas <- read.csv("../../../../Desktop/measurements.csv",
                     header = TRUE)

edm.df.img <- unique(edm.df$Image_ID)
nrow(edm.df)
length(edm.df.img)

edm.meas.img <- unique(edm.meas$Image_ID)
length(edm.meas.img)

setdiff(edm.meas.img, edm.df.img) #all the images with measurements have metadata
setdiff(edm.df.img, edm.meas.img) #these are the images that did not have measurements taken
#these are the images that should be omited

#### INTERMEDIA ----
images.path.int <- "../../../../../../../Volumes/Untitled/microporella-subset-jpg/M_intermedia"
list.int = list.files(path = images.path.int,
                  full.names = TRUE,
                  recursive = TRUE)
list.int.trim <- gsub(list.int,
                  pattern = images.path.int,
                  replacement = "")

list.int.full <- gsub(list.int.trim,
                  pattern = "/omit_intermedia",
                  replacement = "")
list.int.full <- gsub(list.int.full,
                  pattern = "/",
                  replacement = "")
list.int.full <- gsub(list.int.full,
                  pattern = ".jpg",
                  replacement = "")
list.int.full <- gsub(list.int.full,
                      pattern = ".tif",
                      replacement = "")

edm.inter <- edm.df[edm.df$Species == "intermedia",]
nrow(edm.inter) #edm has more images than I do
length(list.int.full)
sort(setdiff(edm.inter$Image_ID, list.int.full)) #lots of edm images

edm.inter.trim <- edm.inter[grep("MHR", edm.inter$Image_ID), ]
nrow(edm.inter.trim) #still has 3 more images than I

setdiff(list.int.full, edm.inter.trim$Image_ID) #no diff between my list and EDM
setdiff(edm.inter.trim$Image_ID, list.int.full) 
#no diff between EDM list and mine

#### SPECULUM ----
images.path.spec <- "../../../../../../../Volumes/Untitled/microporella-subset-jpg/M_speculum"
list.spec = list.files(path = images.path.spec,
                      full.names = TRUE,
                      recursive = TRUE)
list.spec.trim <- gsub(list.spec,
                      pattern = images.path.spec,
                      replacement = "")

list.spec.full <- gsub(list.spec.trim,
                      pattern = "/omit_speculum",
                      replacement = "")
list.spec.full <- gsub(list.spec.full,
                      pattern = "/",
                      replacement = "")
list.spec.full <- gsub(list.spec.full,
                      pattern = ".jpg",
                      replacement = "")
list.spec.full <- gsub(list.spec.full,
                       pattern = ".tif",
                       replacement = "")

edm.spec <- edm.df[edm.df$Species == "speculum",]
nrow(edm.spec) #edm has around 500 more images 
length(list.spec.full)

sort(setdiff(edm.spec$Image_ID, list.spec.full)) #are in my list but not EDM, for me to check that they are M. intermedia and usable

edm.spec.trim <- edm.spec[grep("MHR", edm.spec$Image_ID), ]
nrow(edm.spec.trim) #now has about 15 less images

setdiff(list.spec.full, edm.spec.trim$Image_ID) #no diff between my list and edm
setdiff(edm.spec.trim$Image_ID, list.spec.full) 
#"MHR.9608" are in Emanuela's list but not mine
#this is agonistes in the metadata file but EDM has it as speculum; it looks like agonistes

#### AGONISTES ----
images.path.agon <- "../../../../../../../Volumes/Untitled/microporella-subset-jpg/M_agonistes"
list.agon = list.files(path = images.path.agon,
                       full.names = TRUE,
                       recursive = TRUE)
list.agon.trim <- gsub(list.agon,
                       pattern = images.path.agon,
                       replacement = "")

list.agon.full <- gsub(list.agon.trim,
                       pattern = "/omit_agonistes",
                       replacement = "")
list.agon.full <- gsub(list.agon.full,
                       pattern = "/",
                       replacement = "")
list.agon.full <- gsub(list.agon.full,
                       pattern = ".jpg",
                       replacement = "")
list.agon.full <- gsub(list.agon.full,
                       pattern = ".tif",
                       replacement = "")

edm.agon <- edm.df[edm.df$Species == "agonistes",]
nrow(edm.agon) #edm has around 500 more images 
length(list.agon.full)

setdiff(edm.agon$Image_ID, list.agon.full) 

edm.agon.trim <- edm.agon[grep("MHR", edm.agon$Image_ID), ]
nrow(edm.agon.trim) #now has about 20 more images

setdiff(list.agon.full, edm.agon.trim$Image_ID) #in my list but not EDM
#"LHL.049.BLEED074"  "LHL.050.BLEED074"
setdiff(edm.agon.trim$Image_ID, list.agon.full) #in EDM list but not mine
#"MHR.9210"; cannot find this image on the lab computer

#### DISCORS ----
images.path.disc <- "../../../../../../../Volumes/Untitled/microporella-subset-jpg/M_discors"
list.disc = list.files(path = images.path.disc,
                       full.names = TRUE,
                       recursive = TRUE)
list.disc.trim <- gsub(list.disc,
                       pattern = images.path.disc,
                       replacement = "")

list.disc.full <- gsub(list.disc.trim,
                       pattern = "/omit_discors",
                       replacement = "")
list.disc.full <- gsub(list.disc.full,
                       pattern = "/",
                       replacement = "")
list.disc.full <- gsub(list.disc.full,
                       pattern = ".jpg",
                       replacement = "")
list.disc.full <- gsub(list.disc.full,
                       pattern = ".tif",
                       replacement = "")

edm.disc <- edm.df[edm.df$Species == "discors",]
nrow(edm.disc) #edm has around 200 more images 
length(list.disc.full)

setdiff(edm.disc$Image_ID, list.disc.full) 

edm.disc.trim <- edm.disc[grep("MHR", edm.disc$Image_ID), ]
nrow(edm.disc.trim) #same number!

setdiff(list.disc.full, edm.disc.trim$Image_ID) #In my list but not edm
#"MHR.12481(x50)"    "MHR.12482(x50)"    "MHR.12483(x50)"   
#"MHR.12484(x50)" these are just labeled different
setdiff(edm.disc.trim$Image_ID, list.disc.full) #in edm list but not mine
#MHR.10797" this is agonistes and is in the agonistes folder; it looks like agonistes to me
#"MHR.12481" "MHR.12482" "MHR.12483" "MHR.12484" these are just labeled different



