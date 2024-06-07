# microporella
Extraction of traits from morphological modules of *Microporella* species

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

## Measurements

There are a total of 22 landmarks, numbered 1 to 22.

![landmarks](Microporella_landmarks.png)


Linear measurements were extracted using the landmark coordinates.

*Traits*

<u>Ovicell<\u> (green shading):
Shape:
maximum width: 4O-2O
maximum length: 1O-3O
Area?

Autozooid (white lines, yellow body):
shape measurements similar to what we do for Steginoporella magnifica
zh: 16-9
oh: height between 6, 7, 8
Operculum base length: 7-8
proximal width: 22-10
distal width: 17-15
proximal side length: average between 22-19 and 10-13
distal side length: average between 19-17 and 13-15
zw: 19-13

Ascopore (black lines, lime green shading):
position to operculum: proximal-distal position
position to operculum (right side): 1-7
position to operculum (left side): 1-8
position to side wall: relative position
position to right side wall: 19-1
position to left side wall: 13-1
area?
length? width? 

Avicularia (grey lines, purple shading):
shape measurements
length: 2-5
height: 3-4
position to side wall: relative position
position to left wall: 13-5
position to right wall: 19-2
position to operculum: proximal-distal position
position to operculum (left side): 4-8
position to operculum (right side): 4-7
area?

# Automation

A. Porto created [Steginator](https://github.com/agporto/Steginator), which links [DeepBryo](https://deepbryo.ngrok.io/), a tool developed by [DiMartino et al.](https://www.biorxiv.org/content/early/2022/11/17/2022.11.17.516938) that segments out zooids from a colony, and [ML-morph](https://github.com/agporto/ml-morph), a tool developed by [Porto & Voje](https://doi.org/10.1111/2041-210X.13373) to automatically place landmarks on images.

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

In the code, "[fileNames.R](https://github.com/megbalk/magnifica/blob/main/fileNames.R)", reads in the image names and associated metadata file name and creates the dataset, "[imageList.csv](https://github.com/megbalk/magnifica/blob/main/imageList.csv)". This list retains information about the file structure, parses the image name into the same parts as the "Imaged Steginoporella magnifica specimens.csv", and notes the extension of the data item.

The code also checks for discrepencies being the imaging metadata, "Imaged Steginoporella magnifica specimens.csv", and the list of images.
