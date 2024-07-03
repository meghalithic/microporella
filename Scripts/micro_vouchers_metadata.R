#This script reconciles image names with metadata to ensure that there are no errors
#1. matches images with associated metadata
#2. checks for discrepancies in info

#vouches says if the image is bleached or not
#bleed db has all other sampling information and species identity

#### LOAD PACKAGES ----
source("Scripts/env.R")

#### LOAD DATA ----
bleed.meta <- read.csv("Data/BLEED_DB.csv",
                    header = TRUE)
vouch.meta <- read.csv("Data/Vouchers.csv",
                       header = TRUE)

images.path <- "/home/voje-lab/Desktop/micro-vouchers"

imageNames.tif = list.files(path = images.path,
                            full.names = TRUE,
                            recursive = TRUE)
imageNames.tif.trim <- gsub(imageNames.tif,
                            pattern = paste0(images.path, "/"),
                            replacement = "")
imageNames <- imageNames.tif.trim[grepl("*.tif",
                                        imageNames.tif.trim)]

imageNames <- gsub(imageNames,
                   pattern = ".tif",
                   replacement = "")

#### COMBINE ----
# add in voucher metadata about bleaching to file
# add in bleed sampling metadata to file using bleed no

##### VOUCHER DATA -----

imageNames <- gsub("[(]x80[)]", 
                   "",
                   imageNames)

imageNames <- gsub("[(]x40[)]", 
                   "",
                   imageNames)

imageNames <- gsub("\\.RHABDOP", 
                   "",
                   imageNames)

imageNames <- gsub("1688_RHABDOP", 
                   "1688",
                   imageNames)

length(setdiff(imageNames, vouch.meta$Name_of_file))
length(setdiff(vouch.meta$Name_of_file, imageNames))

#once reconciled, will match

###### BLEED DB DATA FOR SP AND SAMPLING INFO ----
#trim to species of interest
sort(unique(bleed.meta$Genus))
#"Microporella"    "Microporella\n"
bleed.meta$Genus <- gsub(pattern = "\n", replacement = ", ", x = bleed.meta$Genus)

bleed.meta.micro <- bleed.meta[bleed.meta$Genus == "Microporella",]

sort(unique(bleed.meta.micro$Species))
#"intermedia (discors)"  "intermedia?"  "discors?" "agonistes?"  "intermediata"  

sp <- c("agonistes", "discors", "intermedia", "speculum", "intermedia (discors)", "intermedia?", "discors?", "agonistes?", "intermediata")
bleed.meta.micro.trim <- bleed.meta.micro[bleed.meta.micro$Species %in% sp,]

##### MATCH BY BLEED CODE -----
#BLEED_no in vouch.meta
#BLEED_code in bleed.meta
#vouchers will have some images that are not of interest, and bleed will have some that maybe do not have sems
#want to see which sems missing

length(setdiff(bleed.meta.micro.trim$BLEED_code, unique(vouch.meta$BLEED_no)))
bleed.meta.micro.trim[bleed.meta.micro.trim$BLEED_code %in% setdiff(bleed.meta.micro.trim$BLEED_code, unique(vouch.meta$BLEED_no)), c(2, 17)]
#check if these missing ones have SEMs
#BLEED.005 pdt19273, pdt19274, pdt19275, pdt19276
#BLEED 73:LHL.053, LHL.054
#BLEED 74: LHL.049, LHL.050
#BLEED 2119: Microporella speculum Greta Point slipway Wellington 4 BLEED2119.tif, Microporella speculum Greta Point slipway Wellington 1 BLEED2119.tif, Microporella speculum Greta Point slipway Wellington 2 BLEED2119.tif, Microporella speculum Greta Point slipway Wellington 3 BLEED2119.tif
##have all the LHL images

#keep only the vouchers that are the species of interest
bleed.meta.micro.trim2 <- bleed.meta.micro.trim[,c(1:3,5,17,20,23:37)]
vouch.img.trim <- merge(vouch.meta, bleed.meta.micro.trim2,
                        by.x = "BLEED_no", by.y = "BLEED_code",
                        all.x = FALSE, all.y = FALSE)
nrow(vouch.img.trim)
vouch.img.trim$BLEED_no #only 116 and 25

#### IMAGE METADATA ----

#This script reconciles image names with metadata to ensure that there are no errors
#1. gets list of file names and checks for duplicates
#2. extracts metadata from txt files
#3. combines files
#4. makes sure there are no discrepancies in metadata with image info

#### 1. LIST OF FILE NAMES ----

##https://stackoverflow.com/questions/54510134/getting-list-of-file-names-in-a-directory
# To list all files of a folder in a list variable including files 
# from sub-folders. The code below gets the full path of files not just names.
#list = list.files(path = full_path_to_directory ,full.names=TRUE,recursive=TRUE)
# To get names of all files from their corresponding paths in all_names variable.
#all_names = basename(list)
# To write all_names variable to a CSV file.
#write.csv(all_names, "test.csv")

## get folder names
list = list.files(path = images.path,
                  full.names = TRUE,
                  recursive = TRUE)

##### PARSE FILE NAMES -----
listPath <- unlist(list)
length(listPath) 

list.trim <- gsub(list,
                  pattern = images.path,
                  replacement = "")

list.parse <- str_split(list.trim,
                        pattern = "/")

#first folder is either Sara (folder name Sara) or Mali (folder names Stegs and Stegs2)
#subfolder folder, when given, is the formation or grouping

fileName <- c()
ext <- c()

for(i in 1:length(list.parse)){
  fileName[i] <- list.parse[[i]][2]
  if(isTRUE(endsWith(fileName[i], ".txt"))){
    ext[i] <- "txt"
  }
  else{
    ext[i] <- "tif"
  }
}

##### PARSE IMAGE NAME -----

image.list <- str_split(fileName, pattern = "\\.") #for microporella

Image_ID <- c()
specimenNR <- c()
imageNo <- c()

for(i in 1:length(image.list)){
  Image_ID[i] <- paste0(image.list[[i]][1], ".", image.list[[i]][2])
  specimenNR[i] <- paste0(image.list[[i]][3], ".", image.list[[i]][4])
  imageNo[i] <- image.list[[i]][2]
}

##### COMBINE & WRITE CSV ----

df.list <- data.frame(path = listPath,
                      specimenNR = specimenNR,
                      ext = ext,
                      fileName = fileName,
                      Image_ID = Image_ID,
                      imageNo = imageNo,
                      stringsAsFactors = FALSE)

nrow(df.list) #10955
nrow(df.list[df.list$ext == "tif",]) #7333 #should be half; have more txt files than images
sort(table(df.list$Image_ID)) #for microporella

unique(duplicated(df.list$path)) #should all be FALSE

#### 2. EXTRACT METADATA FROM TXT FILES ----

list.txt <- listPath[!grepl("*.tif",
                            listPath)]
length(list.txt) #5481

txtPath <- unlist(list.txt)

##### READ TXT FILES -----

txt.df <- data.frame()

for(i in 1:length(txtPath)){
  f <- read.table(txtPath[i],
                  sep = "^",
                  fileEncoding = "UTF-16",
                  skip = 1)
  
  ## now make two columns, using "=" as deliminator
  
  ff <- cSplit(f, 'V1',
               sep = "=",
               stripWhite = TRUE,
               type.convert = FALSE)
  
  #seems Condition is multiple "="
  condition <- str_split(ff[ff$V1_1 == "Condition",],
                         pattern = "\ ")
  
  av <- c(condition[[2]][1],condition[[3]][1])
  mag <- c(condition[[3]][2], condition[[4]][1])
  wd <- c(condition[[4]][2], condition[[5]][1])
  lensMode <- c(condition[[5]][2], condition[[6]][1])
  path <- c("path", txtPath[i])
  
  cond.paste <- paste(ff$V1_2[ff$V1_1 == "Condition"], 
                      ff$V1_3[ff$V1_1 == "Condition"],
                      ff$V1_4[ff$V1_1 == "Condition"], 
                      ff$V1_5[ff$V1_1 == "Condition"],
                      ff$V1_6[ff$V1_1 == "Condition"], 
                      sep = " ")
  
  ff2 <- ff
  
  ff2$V1_2[ff2$V1_1 == "Condition"] <- cond.paste
  
  ff3 <- ff2[,1:2]
  
  ff4 <- rbind(path, as.data.frame(ff3), av, mag, wd, lensMode)
  
  names <- ff4$V1_1
  ff5 <- as.data.frame(t(ff4[,-1]))
  colnames(ff5) <- names
  
  txt.df <- rbind(txt.df, ff5)
  print(paste0(txtPath[i], " ", i))
  
}

nrow(txt.df) #5482

txt.df$fileName <- basename(txt.df$path)
txt.image.list <- str_split(txt.df$fileName,
                            pattern = "\\.")
txt.df$imageNo <- ""
for(i in 1:length(txt.image.list)){
  txt.df$imageNo[i] <- txt.image.list[[i]][2]
}

#### 3. COMBINE IMAGE AND TEXT FILES ----
## make just images
df.images <- df.list[df.list$ext == "tif",]
df.images$imageName <- gsub(".tif", "", df.images$fileName)

txt.df$imageName <- gsub(".txt", "", txt.df$fileName)

length(setdiff(df.images$fileName, txt.df$ImageName)) #should be none
# 13 images that I need to ask Mali about
length(setdiff(txt.df$ImageName, df.images$fileName)) #should be none
#mostly edm images, will ask Mali

df.image.meta <- merge(df.images, txt.df,
                       by = "imageName",
                       all.x = TRUE, all.y = TRUE)
nrow(df.image.meta)

colnames(df.image.meta)[colnames(df.image.meta) == 'specimenNR.x'] <- 'specimenNR.tif'
colnames(df.image.meta)[colnames(df.image.meta) == 'specimenNR.y'] <- 'specimenNR.txt'
colnames(df.image.meta)[colnames(df.image.meta) == 'fileName.x'] <- 'fileName.tif'
colnames(df.image.meta)[colnames(df.image.meta) == 'fileName.y'] <- 'fileName.txt'
colnames(df.image.meta)[colnames(df.image.meta) == 'path.x'] <- 'path.tif'
colnames(df.image.meta)[colnames(df.image.meta) == 'path.y'] <- 'path.txt'

#### 4. CHECK METADATA AND FILE INFO ----
## make check in ImageName matches fileName
df.image.meta$ImageNameCheck <- df.image.meta$fileName.tif == df.image.meta$ImageName
df.image.meta$ImageName[df.image.meta$ImageNameCheck == FALSE] 
#mostly NAs or ones already identified as a problem

## make check for AV and mag
# extract numbers only from AV and mag
unique(df.image.meta$Mag)

##double check no differences in txt file names
df.list.txt <- df.list[df.list$ext == "txt",]
setdiff(df.list.txt$fileName, txt.df$fileName)
setdiff(txt.df$fileName, df.list.txt$fileName)
# no difference in txt files

#### COMBINE DATA ----


#### WRITE CSV ----
#these are the images to use for ML
write.csv(df.image.meta,
          "Data/voucher.image.imageMetadata.filtered.csv",
          row.names = FALSE)


