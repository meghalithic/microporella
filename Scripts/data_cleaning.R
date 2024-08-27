##1. combine files
##2. match to metadata
###2a. match to vouchers, micro, bleed
###2b. match to the final dataset used by EDM
##3. add appropriate scale, and convert
##4. TO DO: 
###4a. match category with autozooid
###4b. remove outliers
###4c. calculate other measurements like distance from top

#### ENVIRONMENT ----
source("Scripts/env.R")

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
colnames(edm.df)

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

agon.df$len <- agon.df$majorAxis/agon.df$scale
agon.df$wid <- agon.df$minorAxis/agon.df$scale
agon.df$ln.len <- log(agon.df$len)
agon.df$ln.wid <- log(agon.df$wid)
agon.df$area.um <- agon.df$area/agon.df$scale

disc.df$len <- disc.df$majorAxis/disc.df$scale
disc.df$wid <- disc.df$minorAxis/disc.df$scale
disc.df$ln.len <- log(disc.df$len)
disc.df$ln.wid <- log(disc.df$wid)
disc.df$area.um <- disc.df$area/disc.df$scale

int.df$len <- int.df$majorAxis/int.df$scale
int.df$wid <- int.df$minorAxis/int.df$scale
int.df$ln.len <- log(int.df$len)
int.df$ln.wid <- log(int.df$wid)
int.df$area.um <- int.df$area/int.df$scale

spec.df$len <- spec.df$majorAxis/spec.df$scale
spec.df$wid <- spec.df$minorAxis/spec.df$scale
spec.df$ln.len <- log(spec.df$len)
spec.df$ln.wid <- log(spec.df$wid)
spec.df$area.um <- spec.df$area/spec.df$scale

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
#unique(agon.meta$image_id[agon.meta$Species != "agonistes"])
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
head(form.df$Formation_name)
##fix spelling error
form.df$Formation_name[form.df$Formation_name == "Lower Castecliff Shellbed"] <- "Lower Castlecliff Shellbed"
unique(form.df$Formation_name)
#Lower Kai-Iwi = Lower Kai-iwi Shellbed
#TEWKESBURY; Tewkesbury = Tewkesbury Formation
#Upper Kai-Iwi = Upper Kai-iwi Shellbed
#Wahgnanui (core) = Whanganui Core
#LCSB = Lower Castlecliff Shellbed
#SHCS = Shakespeare Cliff Basal Sand Shellbed
#NKBS = Nukumaru Brown Sand
#NKLS = Nukumaru Limestone
#TAINUI SB = Tainui Shellbed
#UCSB = Upper Castlecliff Shellbed
#off Pipi Rocks?

setdiff(agon.meta$Formation, form.df$Formation_name) #missing a lot, maybe spelling?
#unique(agon.meta$Formation)
agon.meta$Formation[agon.meta$Formation == "Lower Kai-iwi"] <- "Lower Kai-iwi Shellbed"
agon.meta$Formation[agon.meta$Formation == "TEWKESBURY"] <- "Tewkesbury Formation"
agon.meta$Formation[agon.meta$Formation == "Tewkesbury"] <- "Tewkesbury Formation"
agon.meta$Formation[agon.meta$Formation == "Upper Kai-iwi"] <- "Upper Kai-iwi Shellbed"
agon.meta$Formation[agon.meta$Formation == "Whanganui (core)"] <- "Whanganui Core"
agon.meta$Formation[agon.meta$Formation == "LCSB"] <- "Lower Castlecliff Shellbed"
agon.meta$Formation[agon.meta$Formation == "SHCS"] <- "Shakespeare Cliff Basal Sand Shellbed"
agon.meta$Formation[agon.meta$Formation == "NKBS"] <- "Nukumaru Brown Sand"
agon.meta$Formation[agon.meta$Formation == "NKLS"] <- "Nukumaru Limestone"
agon.meta$Formation[agon.meta$Formation == "TAINUI SB"] <- "Tainui Shellbed"
agon.meta$Formation[agon.meta$Formation == "UCSB"] <- "Upper Castlecliff Shellbed"
setdiff(agon.meta$Formation, form.df$Formation_name) 

setdiff(disc.df$Formation, form.df$Formation_name)
setdiff(int.df$Formation, form.df$Formation_name)
setdiff(spec.df$Formation, form.df$Formation_name)

agon.meta.form <- merge(agon.meta, form.df,
                        by.x = "Formation", by.y = "Formation_name")
disc.meta.form <- merge(disc.meta, form.df,
                        by.x = "Formation", by.y = "Formation_name")
int.meta.form <- merge(int.meta, form.df,
                       by.x = "Formation", by.y = "Formation_name")
spec.meta.form <- merge(spec.meta, form.df,
                        by.x = "Formation", by.y = "Formation_name")

#### MATCH ZOOIDS ----
#use polygons to match ascopore, avicularia, and orifice to autozooid

#start with one image
images <- unique(agon.meta.form$image_id)
x <- agon.meta.form[agon.meta.form$image_id == images[1],] #MHR.6506
#need to pull out x,y coordinates, give them each a row, with both image_id and box_id, which is id (but retain all three)
#will need to look at overlap by image_id and of box_ids

x.trim <- x %>%
    select(image_id, V1, id, polygon, category)

x.trim$coords.parse <- purrr::map(x.trim$polygon, jsonlite::fromJSON)

str(x.trim$coords.parse) #lists; how do I get these to become rows?

x.trim$coords.parse[[1]][,1]

#this has been the most helpful: https://stackoverflow.com/questions/52669779/convert-sets-of-spatial-coordinates-to-polygons-in-r-using-sf
long.x <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(long.x) <- c('id', 'image_id', 'box_id', 'coords.x', "coords.y")
#make a new dataframe
for(i in 1:nrow(x.trim)){
    y <- do.call(rbind.data.frame, x.trim$coords.parse[i])
    colnames(y) <- c('coords.x', 'coords.y')
    y$id <- x.trim$id[i]
    y$image_id <- x.trim$image_id[i]
    y$box_id <- x.trim$V1[i]
    long.x <- rbind(long.x, y)
}

long.x %>%
    st_as_sf(coords = c("coords.x", "coords.y")) %>%
    group_by(id) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    plot()

#woo! now find overlap

sf <- sfheaders::sf_polygon(
    obj = long.x
    , x = "coords.x"
    , y = "coords.y"
    , polygon_id = "id"
)
sf

plot(sf, col = sf.colors(categorical = TRUE, alpha = .5))
title("overlapping zooids")

diffs <- st_difference(sf)
plot(diffs, col = sf.colors(categorical = TRUE, alpha = .5))
title("non-overlapping differences")

ints = st_intersection(st_make_valid(sf)) # all intersections
plot(ints, col = sf.colors(categorical = TRUE, alpha = .5))
title("non-overlapping intersections")

summary(lengths(st_overlaps(st_make_valid(sf), st_make_valid(sf)))) # includes self-counts!
summary(lengths(st_overlaps(diffs, diffs))) #none?

sfs = st_sf(sf)
int.sfs = st_intersection(st_make_valid(sfs)) # all intersections
plot(int.sfs["n.overlaps"])

#find unions?
st_union(st_make_valid(sf), by_feature = TRUE, is_coverage = FALSE)
#st_union(sf, by_feature = FALSE, is_coverage = FALSE)
st_within(st_make_valid(sfs), sparse = FALSE)

row.within <- c()
col.within <- c()
s.cont <- st_contains(st_make_valid(sfs), sparse = FALSE) #WANT TO USE THIS!!!
#find the true combinations
for(i in 1:nrow(s.cont)){ #rows
    for(j in 1:ncol(s.cont)){ #columns
        if(isTRUE(s.cont[i,j])){
            row.within[i] <- i   
            col.within[i] <- j
    }
    }
}

auto.box <- x.trim$V1[x.trim$category == "autozooid"]


st_intersection(st_make_valid(sf))


#convert to sf object
counter <- unique(long.x$id)
pts.1 <- 
polygon1 <- st_polygon(x = x.trim$coords.parse[1])




coords.agon <- x$polygon[c(1:2)]
str_split(coords.agon[1],
          pattern = "]]") #only have one list
str(coords.agon) #chr

x$coords.parse.x <- c()
x$coords.parse.y <- c()

for(i in 1:nrow(x)){
    x$coords.parse.x[i] <- x$coords.parse[[i]][,1]
    x$coords.parse.y[i] <- x$coords.parse[[i]][,2]
}

agon.meta.form %>%
    st_as_sf(coords = c(x$coords.parse[[1]][,1], x$coords.parse[[1]][,2])) %>%
    group_by(id) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") %>%
    plot()


coords.agon.parse1 <- purrr::map(coords.agon[1], jsonlite::fromJSON)
coords.agon.parse2 <- purrr::map(coords.agon[2], jsonlite::fromJSON)

coords.agon.1 <- fromJSON(coords.agon[1])
coords.agon.2 <- fromJSON(coords.agon[2])

coords_mat1 <- matrix(unlist(coords.agon.parse1), ncol = 2, byrow = TRUE)
coords_mat2 <- matrix(unlist(coords.agon.2), ncol = 2, byrow = TRUE)

coords_mat.list1 <- list(coords_mat1)
polygon1 <- st_polygon(coords_mat.list1)



# Function to parse and ensure closure of coordinates
parse_and_close_coords <- function(coords_json) {
    coords <- fromJSON(coords_json)
    
    # Check if the first and last coordinates are the same
    if (!identical(coords[[1]], coords[[length(coords)]])) {
        coords <- append(coords, list(coords[[1]]))
    }
    
    # Convert to matrix
    coords_mat <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
    
    return(coords_mat)
}

# Parse and close coordinate strings
coords1_parsed <- parse_and_close_coords(coords.agon[1])
coords2_parsed <- parse_and_close_coords(coords.agon[2])

# Create polygons
polygon1 <- st_polygon(list(coords1_parsed))
polygon2 <- st_polygon(coords2_parsed)



geojson_data.agon <- c()
for(i in 1:length(coords.agon)){
    geojson_data.agon[i] <- paste0('{
  "type": "FeatureCollection",
  "features": [
    {
      "type": "Feature",
      "geometry": {
        "type": "Polygon",
        "coordinates": [', coords.agon[i], ']
      },
      "properties": {}
    }
  ]
}')
}


# Read GeoJSON data
sf_data.agon <- geojson_sf(geojson_data.agon)
# Assuming sf_data contains multiple polygons
intersection.agon <- st_intersection(sf_data.agon)


# Parse the coordinates
coords1_parsed <- fromJSON(coords.agon[1])
coords2_parsed <- fromJSON(coords.agon[2])

# Function to parse and ensure closure of coordinates
parse_and_close_coords <- function(coords_json) {
    coords <- fromJSON(coords_json)
    
    # Check if the first and last coordinates are the same
    if (!identical(coords[[1]], coords[[length(coords)]])) {
        coords <- append(coords, list(coords[[1]]))
    }
    
    # Convert to matrix
    coords_mat <- matrix(unlist(coords), ncol = 2, byrow = TRUE)
    
    return(coords_mat)
}

# Parse and close coordinate strings
coords1_parsed <- parse_and_close_coords(coords1)
coords2_parsed <- parse_and_close_coords(coords2)

# Create polygons
polygon1 <- st_polygon(list(coords1_parsed))
polygon2 <- st_polygon(list(coords2_parsed))


# Ensure the polygons are closed by checking if first and last points are the same
if (!identical(coords1_parsed[[1]], coords1_parsed[[length(coords1_parsed)]])) {
    coords1_parsed <- append(coords1_parsed, list(coords1_parsed[[1]]))
}

if (!identical(coords2_parsed[[1]], coords2_parsed[[length(coords2_parsed)]])) {
    coords2_parsed <- append(coords2_parsed, list(coords2_parsed[[1]]))
}

# Create polygons
polygon1 <- st_polygon(list(matrix(unlist(coords1_parsed), 
                                   ncol = 2, byrow = TRUE)))
polygon2 <- st_polygon(list(matrix(unlist(coords2_parsed), 
                                   ncol = 2, byrow = TRUE)))

# Convert to sf objects
sf_polygon1 <- st_sfc(polygon1)
sf_polygon2 <- st_sfc(polygon2)

# Find the union of the polygons
union_polygon <- st_union(sf_polygon1, sf_polygon2)

# Print the union polygon
print(union_polygon)

# Extract coordinates from the union polygon
union_coords <- st_coordinates(union_polygon)

# Convert coordinates to string
union_coords_string <- toJSON(union_coords, auto_unbox = TRUE)



gc.agon <- paste0('{"type":"Point","coordinates":', 
                  agon.meta.form$polygon[1],
                  '}')

sf.agon <- geojson_sf(gc.agon)

# Create a GeoJSON structure
geojson_structure.agon <- list(
    type = "FeatureCollection",
    features = list(
        list(
            type = "Feature",
            geometry = list(
                type = "Polygon",
                coordinates = fromJSON(coords.agon)
            ),
            properties = new.env()  # or list() if you want to add properties
        )
    )
)

# Convert to a JSON string
geojson_string.agon <- toJSON(geojson_structure.agon, 
                         auto_unbox = TRUE, force = TRUE)

sf_object.agon <- geojson_sf(geojson_string.agon)



#list.parse <- str_split(as.list(coords),
#                        pattern = "]]")

for(i in 1:length(coords)){
    geojson_structure[i] <- list(
        type = "FeatureCollection",
        features = list(
            list(
                type = "Feature",
                geometry = list(
                    type = "Polygon",
                    coordinates = list(
                        fromJSON(coords[i])
                    )
                ),
                properties = list()  # Empty properties list, you can add your properties here
            )
        )
    )
}

sf_data <- geojson_sf(geojson_structure)

#Error: parse error: trailing garbage
#           ], [780, 1418], [779, 1417]] [[960, 1839], [955, 1844], [955
#                      (right here) ------^

#y <- purrr::map(x$polygon, jsonlite::fromJSON) #seems to deconstruct all the pairs of coordinates
#x$polygon[1]
#[1] "[[777, 1417], [776, 1418], [759, 1418], [758, 1419]
#y[1]
#[[1]]
#[,1] [,2]
#[1,]  777 1417
#[2,]  776 1418
#[3,]  759 1418
#[4,]  758 1419

#z <- jsonlite::stream_in(textConnection(x$polygon))
#doesn't work because differing lengths

#aa <- lapply(paste0("[",x$polygon,"]"), function(x) jsonlite::fromJSON(x))
#totally deconstructs it

list_data <- y
# Function to convert the list to JSON string
list_to_json_string <- function(list_data) {
    return(toJSON(list_data, auto_unbox = TRUE))
}

# Convert the list
json_string <- list_to_json_string(list_data)

# Read GeoJSON data
geojson_structure <- list(
    type = "FeatureCollection",
    features = list(
        list(
            type = "Feature",
            geometry = list(
                type = "Polygon",
                coordinates = list(
                    fromJSON(json_string) #coords #y
                )
            ),
            properties = list()  # Empty properties list, you can add your properties here
        )
    )
)

#geojson_data <- unlist(geojson_structure)

#sf_data <- geojson_sf(geojson_data) #can't take a list

# Convert to a JSON string
geojson_string <- c()
for(i in 1:length(geojson_structure)){
    geojson_string[i] <- toJSON(geojson_structure[i], auto_unbox = TRUE)
}

sf_object <- geojson_sf(geojson_string[1])

st_intersects(sf_object)

#### MAKE SURE EACH COLONY HAS AT LEAST 3 OF EVERY CATEGORY ----

#### SAVE DATA ----
cli.meta.form.list = list(agon.meta.form,
                          disc.meta.form,
                          int.meta.form,
                          spec.meta.form)
save(cli.meta.form.list,
     file = "Data/cli.meta.form.list.RData")
