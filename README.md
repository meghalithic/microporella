# microporella
Extraction of traits from morphological modules of _Microporella_ species

# Images

The images are SEM images of _Microporella_ collected from New Zealand. These specimens are both modern and paleo (~2.3 Mya).

The images were collected by K. Voje, L.H. Liow, E. Di Martino, and others as part of the WABO expeditions. The specimens were imaged by M. Ramsfjell and E. Di Martino.

Images are stored on a shared lab computer and will be made available with the publication of this project.

# Metadata

## Imaging metadata

The metadata file "[Microporella_SEMs_EDM+Mali_05.06.2024.csv](https://github.com/megbalk/microporella/blob/main/Data/Microporella_SEMs_EDM%2BMali_05.06.2024.csv)" contains information about:
- Date: date of image in MM/DD/YYY format
- Image_ID: a unique specimen number
- Formation: formation from which specimens came
- Age: age of the formation in stages, not years
- Sample_ID: unique number assigned to collection sample
- Shell: unique number assigned to shell within collection sample (Sample_ID)
- Colony: unique number assigned to the colony on the shell (can be multiple on one shell)
- Genus: genus of the binomial
- Species: species of the binomial

# Traits

We extracted linear measurements from landmarks images of zooids.

The example is from [Di Martino et al. 2023](https://doi.org/10.1002/lom3.10563) and is of _Microporella discors_.

## Measurements

There are a total of 22 landmarks, numbered 1 to 22.

![landmarks](Microporella_landmarks.png)

Linear measurements were extracted using the landmark coordinates.

*Traits*

**Ovicell** (green shading):
  Shape:
    maximum width: 4O-2O
    maximum length: 1O-3O
    area

**Autozooid** (yellow shading, white lines):
  Shape:
    zooid height: 16-9
    operculum (pink shading): height between 6, 7, 8
    operculum base length (pink shading): 7-8
    proximal width: 22-10
    distal width: 17-15
    proximal side length: average between 22-19 and 10-13
    distal side length: average between 19-17 and 13-15
    zooid width: 19-13
    area

**Ascopore** (lime green shading, black lines):
  Position to operculum: proximal-distal position
    position to operculum (right side): 1-7
    position to operculum (left side): 1-8
  Position to side wall: relative position
    position to right side wall: 19-1
    position to left side wall: 13-1
  Shape:
    area

**Avicularia** (purple shading, grey lines):
  Shape:
    length: 2-5
    height: 3-4
    area
  Position to operculum: proximal-distal position
    position to operculum (left side): 4-8
    position to operculum (right side): 4-7
  Position to side wall: relative position
    position to left wall: 13-5
    position to right wall: 19-2

# Automation

We use two steps for gathering linear measurements:

1. [DeepBryo](https://github.com/agporto/DeepBryo/), a tool developed by [Di Martino et al. 2023](https://doi.org/10.1002/lom3.10563) and which we forked for our project ([DeepBryo_micro](https://github.com/megbalk/DeepBryo_micro))
  - this provides segmentation of morphological features of _Microporella_ colonies
  - the program also extracts area

2.  [ML-morph[(https://github.com/agporto/ml-morph) a tool developed by [Porto & Voje 2020](https://doi.org/10.1111/2041-210X.13373) and forked for our project ([ml-morph_micro](https://github.com/megbalk/ml-morph_micro))
   - this tool automatically places landmarks on images after a bit of training

The output of the machine learning pipeline is a csv file of:
- id of the image (specimenNR_NR of pics_AV_magnification_backscatter)
- box_id, a unique identifier for the zooid in the image (box_top_box_left_box_width_box_height)
- box_top
- box_left
- box_width
- box_height
- Xn cooridnate, where n corresponds to the landmark number for landmarks 0-22
- Yn coordinate, where n corresponds to the landmark number for landmarks 0-22

# Data processing

We check the metadata for every image in the scripts [microporella_imageMetadata.R](https://github.com/megbalk/microporella/blob/main/Scripts/microporella_imageMetadata.R) and [microporella_metadata.R](https://github.com/megbalk/microporella/blob/main/Scripts/microporella_metadata.R) to restrict images that have the same magnification (x50) and remove any duplicates.

In the code, "[fileNames.R](https://github.com/megbalk/microporella/blob/main/Scripts/filterImages.R)", reads in the image names and associated metadata file name and creates the dataset, "[image.filter.csv](https://github.com/megbalk/microporella/blob/main/Data/image.filter.csv)".
