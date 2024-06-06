#This script reconciles image names with metadata to ensure that there are no errors
#1. gets list of file names and checks for duplicates
#2. extracts metadata from txt files
#3. combines files
#4. makes sure there are no discrepancies in metadata with image info

#### LOAD PACKAGES ----
source("Scripts/env.R")

#### GET PATH -----
images.path <- "/home/voje-lab/Desktop/Microporella"

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

folder <- c()
subfolder <- c()
fileName <- c()
ext <- c()

for(i in 1:length(list.parse)){
  folder[i] <- list.parse[[i]][1]
  if(isTRUE(endsWith(list.parse[[i]][2], ".txt"))){
    fileName[i] <- list.parse[[i]][2]
    subfolder[i] <- "NONE"
  }
  else if(isTRUE(endsWith(list.parse[[i]][2], ".tif"))){
    fileName[i] <- list.parse[[i]][2]
    subfolder[i] <- "NONE"
  }
  else{
    subfolder[i] <- list.parse[[i]][2]
    fileName[i] <- list.parse[[i]][3]
  }
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

for(i in 1:length(image.list)){
  Image_ID[i] <- paste0(image.list[[i]][1], ".", image.list[[i]][2])
  specimenNR[i] <- image.list[[i]][2]
}

##### COMBINE & WRITE CSV ----

df.list <- data.frame(path = listPath,
                      folder = folder,
                      subfolder = subfolder,
                      image = specimenNR,
                      ext = ext,
                      fileName = fileName,
                      Image_ID = Image_ID,
                      stringsAsFactors = FALSE)

nrow(df.list) #14694
nrow(df.list[df.list$ext == "tif",]) #7333 #should be half; have more txt files than images
sort(table(df.list$Image_ID)) #for microporella

unique(duplicated(df.list$fileName)) #should all be FALSE

#### 2. EXTRACT METADATA FROM TXT FILES ----

list.txt <- listPath[!grepl("*.tif",
                            listPath)]
length(list.txt)

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

nrow(txt.df) #7358

txt.df$fileName <- basename(txt.df$path)
txt.image.list <- str_split(txt.df$fileName,
                            pattern = "\\.")
txt.df$image <- ""
for(i in 1:length(txt.image.list)){
  txt.df$image[i] <- txt.image.list[[i]][2]
}

#### 3. COMBINE IMAGE AND TEXT FILES ----
## make just images
df.images <- df.list[df.list$ext == "tif",]

length(setdiff(df.images$fileName, txt.df$ImageName)) #should be none
# missing txt files for 12650 and 6270; fixed manually, they had "tif" in the image name
length(setdiff(txt.df$ImageName, df.images$fileName)) #should be none
#missing tif for lots...

df.image.meta <- merge(df.images, txt.df,
                       by = "image",
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
#check for false

## make check for AV and mag
# extract numbers only from AV and mag

df.image.meta$magCheck <- df.image.meta$Mag == "x50"
#check for false

df.image.meta$AVCheck <- df.image.meta$Vacc == "15.0kV"
#check for false

df.image.meta.trim <- df.image.meta[df.image.meta$magCheck == TRUE,]
#omit any false
df.image.meta.trim <- df.image.meta.trim[df.image.meta.trim$AVCheck == TRUE,]

nrow(df.image.meta) #7360
nrow(df.image.meta.trim)#7324, only lost 36 images

##double check no differences in txt file names
df.list.txt <- df.list[df.list$ext == "txt",]
setdiff(df.list.txt$fileName, txt.df$fileName)
setdiff(txt.df$fileName, df.list.txt$fileName)
# no difference in txt files

#### WRITE CSV ----
#these are the images to use for ML
write.csv(df.image.meta.trim,
          "Data/image.imageMetadata.filtered.csv",
          row.names = FALSE)
#lost 36 images


