require(purrr)
require(data.table)

#### LOAD DATA ----
csv.list = list.files(path = "Data/DeepBryo_outputs/M.intermedia_outputs/",
                      pattern = "\\.csv$",
                      full.names = TRUE,
                      recursive = TRUE)

csv.files = lapply(csv.list, read.csv)

df <- do.call(rbind, lapply(csv.list, function(x) fread(x, stringsAsFactors = FALSE)))

length(unique(df$image_id)) #17

rm.df <- read.csv("Data/DeepBryo_outputs/image_id_with_object_ids_to_remove.csv",
                  header = TRUE)

nrow(rm.df)
length(unique(rm.df$image_id)) #38

rm.img <- unique(rm.df$image_id)
img <- unique(df$image_id)

setdiff(img, rm.img) #all are different??
setdiff(rm.img, img) #all are different??
