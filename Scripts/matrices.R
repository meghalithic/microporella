##1. plot traits
##2. create P matrices
##3. create G matrices
##4. look at matrices over time
##5. compare directions of G and ∆z
##6. create ancestral G matrix (SPECIES)
##7. compare directions of ∆z and ancestral G

#### ENVIRONMENT ----
source("Scripts/env.R")

#### LOAD DATA ----
load(file = "Data/cli.meta.form.list.RData") #load the g matrices calculated above 

agon.meta.form <- cli.meta.form.list[[1]]
disc.meta.form <- cli.meta.form.list[[2]]
int.meta.form <- cli.meta.form.list[[3]]
spec.meta.form <- cli.meta.form.list[[4]]

#### MANIPULATE DATA ----
agon.zooid.list <- unique(agon.meta.form$id)
length(agon.zooid.list) #51307

disc.zooid.list <- unique(disc.meta.form$id)
length(disc.zooid.list) #27448

int.zooid.list <- unique(int.meta.form$id)
length(int.zooid.list) #9444

spec.zooid.list <- unique(spec.meta.form$id)
length(spec.zooid.list) #42613

agon.colony.list <- unique(agon.meta.form$col.id)
length(agon.colony.list) #312

disc.colony.list <- unique(disc.meta.form$col.id)
length(disc.colony.list) #120

int.colony.list <- unique(int.meta.form$col.id)
length(int.colony.list) #74

spec.colony.list <- unique(spec.meta.form$col.id)
length(spec.colony.list) #260

##### SET UP DATASET -----
traits = names(agon.meta.form[, c("ln.len", "ln.wid" )])
length(traits) #2

cat.keep <- c("autozooid", "avicularium", "ovicell")

agon.trim <- agon.meta.form[agon.meta.form$category %in% cat.keep,]
names(agon.trim)

agon.mat <- agon.trim %>%
    dplyr::select(time, id, category, col.id, Sample_ID, matches(traits))

#agon.long <- agon.trim
#agon.long$auto.len.ln <- NA
#agon.long$auto.wid.ln <- NA
#agon.long$avic.len.ln <- NA
#agon.long$avic.wid.ln <- NA
#agon.long$ovi.len.ln <- NA
#agon.long$ovi.wid.ln <- NA

#agon.long$auto.len.ln[agon.long$category == "autozooid"] <- agon.long$ln.len[agon.long$category == "autozooid"]
#agon.long$auto.wid.ln[agon.long$category == "autozooid"] <- agon.long$ln.wid[agon.long$category == "autozooid"]
#agon.long$avic.len.ln[agon.long$category == "avicularium"] <- agon.long$ln.len[agon.long$category == "avicularium"]
#agon.long$avic.wid.ln[agon.long$category == "avicularium"] <- agon.long$ln.wid[agon.long$category == "avicularium"]
#agon.long$ovi.len.ln[agon.long$category == "ovicell"] <- agon.long$ln.len[agon.long$category == "ovicell"]
#agon.long$ovi.wid.ln[agon.long$category == "ovicell"] <- agon.long$ln.wid[agon.long$category == "ovicell"]

#names(agon.long)
#traits.2 = names(agon.long[, c("auto.len.ln", "auto.wid.ln", "avic.len.ln",
#                               "avic.wid.ln", "ovi.len.ln", "ovi.wid.ln")])
#length(traits.2) #6

#agon.mat.2 <- agon.long %>%
#    dplyr::select(Formation, id, category, col.id, matches(traits.2))

#colNums.2 <- match(c(traits.2, "id"), names(agon.mat.2))

disc.trim <- disc.meta.form[disc.meta.form$category %in% cat.keep,]
names(disc.trim)

disc.mat <- disc.trim %>%
    dplyr::select(time, id, category, col.id, Sample_ID, matches(traits))


int.trim <- int.meta.form[int.meta.form$category %in% cat.keep,]
names(int.trim)

int.mat <- int.trim %>%
    dplyr::select(time, id, category, col.id, Sample_ID, matches(traits))


spec.trim <- spec.meta.form[spec.meta.form$category %in% cat.keep,]
names(spec.trim)

spec.mat <- spec.trim %>%
    dplyr::select(time, id, category, col.id, Sample_ID, matches(traits))

agon.mat = as.data.frame(agon.mat)
#agon.mat.2 = as.data.frame(agon.mat.2)
disc.mat = as.data.frame(disc.mat)
int.mat = as.data.frame(int.mat)
spec.mat = as.data.frame(spec.mat)

#### MEANS ----
agon_mean_by_formation_colony = agon.mat %>% #use this going forward
    dplyr::group_by(time, col.id) %>%
    dplyr::summarize(n.zooid = length(id),
                     
                     avg.len = mean(ln.len, na.rm = T),
                     sd.len = sd(ln.len, na.rm = T),
                     
                     avg.wid = mean(ln.wid, na.rm = T),
                     sd.wid = sd(ln.wid, na.rm = T)) %>%
    as.data.frame()
min(agon_mean_by_formation_colony$n.zooid) #15

disc_mean_by_formation_colony = disc.mat %>% #use this going forward
    dplyr::group_by(time, col.id) %>%
    dplyr::summarize(n.zooid = length(id),
                     
                     avg.len = mean(ln.len, na.rm = T),
                     sd.len = sd(ln.len, na.rm = T),
                     
                     avg.wid = mean(ln.wid, na.rm = T),
                     sd.wid = sd(ln.wid, na.rm = T)) %>%
    as.data.frame()
min(disc_mean_by_formation_colony$n.zooid) #14

int_mean_by_formation_colony = int.mat %>% #use this going forward
    dplyr::group_by(time, col.id) %>%
    dplyr::summarize(n.zooid = length(id),
                     
                     avg.len = mean(ln.len, na.rm = T),
                     sd.len = sd(ln.len, na.rm = T),
                     
                     avg.wid = mean(ln.wid, na.rm = T),
                     sd.wid = sd(ln.wid, na.rm = T)) %>%
    as.data.frame()
min(int_mean_by_formation_colony$n.zooid) #23

spec_mean_by_formation_colony = spec.mat %>% #use this going forward
    dplyr::group_by(time, col.id) %>%
    dplyr::summarize(n.zooid = length(id),
                     
                     avg.len = mean(ln.len, na.rm = T),
                     sd.len = sd(ln.len, na.rm = T),
                     
                     avg.wid = mean(ln.wid, na.rm = T),
                     sd.wid = sd(ln.wid, na.rm = T)) %>%
    as.data.frame()
min(spec_mean_by_formation_colony$n.zooid) #12

agon_mean_by_formation_colony_cat = agon.mat %>% #use this going forward
    dplyr::group_by(time, col.id, category) %>%
    dplyr::summarize(n.zooid = length(id),
                     
                     avg.len = mean(ln.len, na.rm = T),
                     sd.len = sd(ln.len, na.rm = T),
                     
                     avg.wid = mean(ln.wid, na.rm = T),
                     sd.wid = sd(ln.wid, na.rm = T)) %>%
    as.data.frame()
table(agon_mean_by_formation_colony_cat[agon_mean_by_formation_colony_cat$n.zooid < 3,
                                        c(1, 3)])
#mostly ovicell and 1 avicularium
#for the by type, let's remove these ones
rm.agon <- agon_mean_by_formation_colony_cat$col.id[agon_mean_by_formation_colony_cat$n.zooid < 3]
agon.mat.cat <- agon.mat[!(agon.mat$col.id %in% rm.agon),]
agon.mat.cat %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(n.col = length(unique(col.id)))
#remove the ones with too few n.zoo
#Tewkesbury Formation, Upper Castlecliff Shellbed

agon.mat.cat <- agon.mat.cat[agon.mat.cat$time != "Tewkesbury Formation"
                             & agon.mat.cat$time != "Upper Castlecliff Shellbed",]
unique(agon.mat.cat$time)

disc_mean_by_formation_colony_cat = disc.mat %>% #use this going forward
    dplyr::group_by(time, col.id, category) %>%
    dplyr::summarize(n.zooid = length(id),
                     
                     avg.len = mean(ln.len, na.rm = T),
                     sd.len = sd(ln.len, na.rm = T),
                     
                     avg.wid = mean(ln.wid, na.rm = T),
                     sd.wid = sd(ln.wid, na.rm = T)) %>%
    as.data.frame()
table(disc_mean_by_formation_colony_cat[disc_mean_by_formation_colony_cat$n.zooid < 3,
                                        c(1, 3)])
#only ovicell
#for the by type, let's remove these ones
rm.disc <- disc_mean_by_formation_colony_cat$col.id[disc_mean_by_formation_colony_cat$n.zooid < 3]
disc.mat.cat <- disc.mat[!(disc.mat$col.id %in% rm.disc),]
disc.mat.cat %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(n.col = length(unique(col.id)))
#remove the ones with too few n.zoo
#Tewkesbury Formation, Butlers Shell Conglomerate, Nukumaru Limestone

disc.mat.cat <- disc.mat.cat[disc.mat.cat$time != "Tewkesbury Formation"
                             & disc.mat.cat$time != "Butlers Shell Conglomerate"
                             & disc.mat.cat$time != "Nukumaru Limestone",]
unique(disc.mat.cat$time)

int_mean_by_formation_colony_cat = int.mat %>% #use this going forward
    dplyr::group_by(time, col.id, category) %>%
    dplyr::summarize(n.zooid = length(id),
                     
                     avg.len = mean(ln.len, na.rm = T),
                     sd.len = sd(ln.len, na.rm = T),
                     
                     avg.wid = mean(ln.wid, na.rm = T),
                     sd.wid = sd(ln.wid, na.rm = T)) %>%
    as.data.frame()
table(int_mean_by_formation_colony_cat[int_mean_by_formation_colony_cat$n.zooid < 3,
                                        c(1, 3)])
#only ovicell
#for the by type, let's remove these ones
rm.int <- int_mean_by_formation_colony_cat$col.id[int_mean_by_formation_colony_cat$n.zooid < 3]
int.mat.cat <- int.mat[!(int.mat$col.id %in% rm.int),]
int.mat.cat %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(n.col = length(unique(col.id)))
#remove the ones with too few n.zoo
#Tainui Shellbed, Lower Castlecliff Shellbed
#NOTE: leaves only ONE formation

int.mat.cat <- int.mat.cat[int.mat.cat$time != "Tainui Shellbed"
                           & int.mat.cat$time != "Lower Castlecliff Shellbed",]
unique(int.mat.cat$time)

spec_mean_by_formation_colony_cat = spec.mat %>% #use this going forward
    dplyr::group_by(time, col.id, category) %>%
    dplyr::summarize(n.zooid = length(id),
                     
                     avg.len = mean(ln.len, na.rm = T),
                     sd.len = sd(ln.len, na.rm = T),
                     
                     avg.wid = mean(ln.wid, na.rm = T),
                     sd.wid = sd(ln.wid, na.rm = T)) %>%
    as.data.frame()
table(spec_mean_by_formation_colony_cat[spec_mean_by_formation_colony_cat$n.zooid < 3,
                                        c(1, 3)])
#only ovicell
#for the by type, let's remove these ones
rm.spec <- spec_mean_by_formation_colony_cat$col.id[spec_mean_by_formation_colony_cat$n.zooid < 3]
spec.mat.cat <- spec.mat[!(spec.mat$col.id %in% rm.spec),]
spec.mat.cat %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(n.col = length(unique(col.id)))
#remove the ones with too few n.zoo
#Upper Castlecliff Shellbed, Upper Westmere Shellbed

spec.mat.cat <- spec.mat.cat[spec.mat.cat$time != "Upper Castlecliff Shellbed"
                           & spec.mat.cat$time != "Upper Westmere Shellbed",]
unique(spec.mat.cat$time)

mean.list = list(agon_mean_by_formation_colony,
                 disc_mean_by_formation_colony,
                 int_mean_by_formation_colony,
                 spec_mean_by_formation_colony)
save(mean.list,
     file = "Data/mean.list.RData")

#### REDUCE TO COLUMNS OF INTEREST ----
trt_lg_N = c("time", "col.id", "id", "category", "Sample_ID", traits)
dat_lg_N_agon = agon.mat[intersect(colnames(agon.mat), trt_lg_N)] #use this
head(dat_lg_N_agon) #traits in same order as df and traits

#trt_lg_N.2 = c("Formation", "col.id", "id", "category", traits.2)
#dat_lg_N.2 = agon.mat.2[intersect(colnames(agon.mat.2), trt_lg_N.2)] #use this
#head(dat_lg_N.2) #traits in same order as df and traits

dat_lg_N_disc = disc.mat[intersect(colnames(disc.mat), trt_lg_N)] #use this
head(dat_lg_N_disc) #traits in same order as df and traits

dat_lg_N_int = int.mat[intersect(colnames(int.mat), trt_lg_N)] #use this
head(dat_lg_N_int) #traits in same order as df and traits

dat_lg_N_spec = spec.mat[intersect(colnames(spec.mat), trt_lg_N)] #use this
head(dat_lg_N_spec) #traits in same order as df and traits

#### CHECK SAMPLE SIZES ----
## number of zooids per colony
range(agon_mean_by_formation_colony$n.zooid)
#15 643

#range(mean_by_formation_colony.2$n.zooid)
#64 1054

range(disc_mean_by_formation_colony$n.zooid)
#14 990

range(int_mean_by_formation_colony$n.zooid)
#23 327

range(spec_mean_by_formation_colony$n.zooid)
#12 494

##### FORMATIONS -----
#oldest to youngest
agon.mat$time <- factor(agon.mat$time,
                        levels = c("Nukumaru Limestone",
                                   "Nukumaru Brown Sand",
                                   "Tewkesbury Formation",
                                   "Lower Kai-iwi Shellbed",
                                   "Upper Kai-iwi Shellbed",
                                   "Lower Castlecliff Shellbed",
                                   "Tainui Shellbed",
                                   "Shakespeare Cliff Basal Sand Shellbed",
                                   "Upper Castlecliff Shellbed",
                                   "modern"))

disc.mat$time <- factor(disc.mat$time,
                        levels = c("Nukumaru Limestone",
                                   "Nukumaru Brown Sand",
                                   "Tewkesbury Formation",
                                   "Butlers Shell Conglomerate",
                                   "Lower Kai-iwi Shellbed",
                                   "Upper Kai-iwi Shellbed",
                                   "Tainui Shellbed",
                                   "modern"))

int.mat$time <- factor(int.mat$time,
                       levels = c("Lower Castlecliff Shellbed",
                                  "Tainui Shellbed",
                                  "Shakespeare Cliff Basal Sand Shellbed",
                                  "modern"))

spec.mat$time <- factor(spec.mat$time,
                        levels = c("Upper Westmere Shellbed",
                                   "Upper Kai-iwi Shellbed",
                                   "Lower Castlecliff Shellbed",
                                   "Tainui Shellbed",
                                   "Shakespeare Cliff Basal Sand Shellbed",
                                   "Upper Castlecliff Shellbed",
                                   "modern"))

##### PLOT TRAITS -----

##### agonistes ----
p.agon.auto.len = ggplot(agon.mat[agon.mat$category == "autozooid",]) + 
    geom_density(aes(x = agon.mat[agon.mat$category == "autozooid", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. agonistes")," autozooid"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = agon.col.form)

p.agon.auto.wid = ggplot(data = agon.mat[agon.mat$category == "autozooid",]) + 
    geom_density(aes(x = agon.mat[agon.mat$category == "autozooid", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. agonistes")," autozooid"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = agon.col.form)

p.agon.avic.len = ggplot(agon.mat[agon.mat$category == "avicularium",]) + 
    geom_density(aes(x = agon.mat[agon.mat$category == "avicularium", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. agonistes")," avicularium"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = agon.col.form)

p.agon.avic.wid = ggplot(data = agon.mat[agon.mat$category == "avicularium",]) + 
    geom_density(aes(x = agon.mat[agon.mat$category == "avicularium", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. agonistes")," avicularium"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = agon.col.form)

p.agon.ovi.len = ggplot(agon.mat[agon.mat$category == "ovicell",]) + 
    geom_density(aes(x = agon.mat[agon.mat$category == "ovicell", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. agonistes")," ovicell"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = agon.col.form)

p.agon.ovi.wid = ggplot(data = agon.mat[agon.mat$category == "ovicell",]) + 
    geom_density(aes(x = agon.mat[agon.mat$category == "ovicell", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. agonistes")," ovicell"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = agon.col.form)


agon.fig = list(p.agon.auto.len, p.agon.avic.len,
                p.agon.ovi.len,
                p.agon.auto.wid, p.agon.avic.wid, 
                p.agon.ovi.wid)
agon.ml <- marrangeGrob(agon.fig, nrow = 3, ncol = 2)
agon.ml
#see a really big shift between Tewkesbury and earlier and Upper Kai-Iwi and after

ggsave(agon.ml, 
       file = "./Results/agon.trait.distribution.png", 
       width = 14, height = 20, units = "cm")

##### discors ----
p.disc.auto.len = ggplot(disc.mat[disc.mat$category == "autozooid",]) + 
    geom_density(aes(x = disc.mat[disc.mat$category == "autozooid", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. discors")," autozooid"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = disc.col.form)

p.disc.auto.wid = ggplot(data = disc.mat[disc.mat$category == "autozooid",]) + 
    geom_density(aes(x = disc.mat[disc.mat$category == "autozooid", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. discors")," autozooid"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = disc.col.form)

p.disc.avic.len = ggplot(disc.mat[disc.mat$category == "avicularium",]) + 
    geom_density(aes(x = disc.mat[disc.mat$category == "avicularium", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. discors")," avicularium"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = disc.col.form)

p.disc.avic.wid = ggplot(data = disc.mat[disc.mat$category == "avicularium",]) + 
    geom_density(aes(x = disc.mat[disc.mat$category == "avicularium", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. discors")," avicularium"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = disc.col.form)

p.disc.ovi.len = ggplot(disc.mat[disc.mat$category == "ovicell",]) + 
    geom_density(aes(x = disc.mat[disc.mat$category == "ovicell", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. discors")," ovicell"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = disc.col.form)

p.disc.ovi.wid = ggplot(data = disc.mat[disc.mat$category == "ovicell",]) + 
    geom_density(aes(x = disc.mat[disc.mat$category == "ovicell", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. discors")," ovicell"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = disc.col.form)


disc.fig = list(p.disc.auto.len, p.disc.avic.len,
                p.disc.ovi.len,
                p.disc.auto.wid, p.disc.avic.wid, 
                p.disc.ovi.wid)
disc.ml <- marrangeGrob(disc.fig, nrow = 3, ncol = 2)
disc.ml
#see a really big shift between Tewkesbury and earlier and Upper Kai-Iwi and after

ggsave(disc.ml, 
       file = "./Results/disc.trait.distribution.png", 
       width = 14, height = 20, units = "cm")

##### intermedia ----
p.int.auto.len = ggplot(int.mat[int.mat$category == "autozooid",]) + 
    geom_density(aes(x = int.mat[int.mat$category == "autozooid", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. intermedia")," autozooid"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = int.col.form)

p.int.auto.wid = ggplot(data = int.mat[int.mat$category == "autozooid",]) + 
    geom_density(aes(x = int.mat[int.mat$category == "autozooid", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. intermedia")," autozooid"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = int.col.form)

p.int.avic.len = ggplot(int.mat[int.mat$category == "avicularium",]) + 
    geom_density(aes(x = int.mat[int.mat$category == "avicularium", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. intermedia")," avicularium"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = int.col.form)

p.int.avic.wid = ggplot(data = int.mat[int.mat$category == "avicularium",]) + 
    geom_density(aes(x = int.mat[int.mat$category == "avicularium", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. intermedia")," avicularium"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = int.col.form)

p.int.ovi.len = ggplot(int.mat[int.mat$category == "ovicell",]) + 
    geom_density(aes(x = int.mat[int.mat$category == "ovicell", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. intermedia")," ovicell"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = int.col.form)

p.int.ovi.wid = ggplot(data = int.mat[int.mat$category == "ovicell",]) + 
    geom_density(aes(x = int.mat[int.mat$category == "ovicell", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. intermedia")," ovicell"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = int.col.form)

int.fig = list(p.int.auto.len, p.int.avic.len,
                p.int.ovi.len,
                p.int.auto.wid, p.int.avic.wid, 
                p.int.ovi.wid)
int.ml <- marrangeGrob(int.fig, nrow = 3, ncol = 2)
int.ml
#see a really big shift between Tewkesbury and earlier and Upper Kai-Iwi and after

ggsave(int.ml, 
       file = "./Results/int.trait.distribution.png", 
       width = 14, height = 20, units = "cm")

##### speculum ----
p.spec.auto.len = ggplot(spec.mat[spec.mat$category == "autozooid",]) + 
    geom_density(aes(x = spec.mat[spec.mat$category == "autozooid", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. speculum")," autozooid"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = spec.col.form)

p.spec.auto.wid = ggplot(data = spec.mat[spec.mat$category == "autozooid",]) + 
    geom_density(aes(x = spec.mat[spec.mat$category == "autozooid", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. speculum")," autozooid"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = spec.col.form)

p.spec.avic.len = ggplot(spec.mat[spec.mat$category == "avicularium",]) + 
    geom_density(aes(x = spec.mat[spec.mat$category == "avicularium", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. speculum")," avicularium"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = spec.col.form)

p.spec.avic.wid = ggplot(data = spec.mat[spec.mat$category == "avicularium",]) + 
    geom_density(aes(x = spec.mat[spec.mat$category == "avicularium", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. speculum")," avicularium"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = spec.col.form)

p.spec.ovi.len = ggplot(spec.mat[spec.mat$category == "ovicell",]) + 
    geom_density(aes(x = spec.mat[spec.mat$category == "ovicell", traits[1]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. speculum")," ovicell"))) + 
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = spec.col.form)

p.spec.ovi.wid = ggplot(data = spec.mat[spec.mat$category == "ovicell",]) + 
    geom_density(aes(x = spec.mat[spec.mat$category == "ovicell", traits[2]], 
                     group = time,
                     col = time)) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. speculum")," ovicell"))) + 
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = spec.col.form)


spec.fig = list(p.spec.auto.len, p.spec.avic.len,
               p.spec.ovi.len,
               p.spec.auto.wid, p.spec.avic.wid, 
               p.spec.ovi.wid)
spec.ml <- marrangeGrob(spec.fig, nrow = 3, ncol = 2)
spec.ml
#see a really big shift between Tewkesbury and earlier and Upper Kai-Iwi and after

ggsave(spec.ml, 
       file = "./Results/spec.trait.distribution.png", 
       width = 14, height = 20, units = "cm")

#### SPLIT BY FORMATION ----

##### agonistes ----
#check number of zooids NOT colonies:
# by colonies use mean_by_formation_colony
# by zooid us dat_lg_N
col_form_agon = split.data.frame(agon_mean_by_formation_colony,  #by colonies
                                 agon_mean_by_formation_colony$time) #zooids per formation
#just to look
col_form_agon.n = lapply(col_form_agon, function(x){dim(x)[1]})

## by zooids:
by_form_agon = split.data.frame(dat_lg_N_agon, 
                                dat_lg_N_agon$time)
#just to look
by_form_agon.n = lapply(by_form_agon, function(x){dim(x)[1]})
form_data_agon = lapply(by_form_agon, function(x) x[complete.cases(x),])

#make sampling matrix
agon.time <- levels(agon.mat$time)
length(agon.time)
agon.col.samp <- c(col_form_agon.n[[1]], col_form_agon.n[[2]], col_form_agon.n[[3]],
                   col_form_agon.n[[4]], col_form_agon.n[[5]], col_form_agon.n[[6]],
                   col_form_agon.n[[7]], col_form_agon.n[[8]], col_form_agon.n[[9]],
                   col_form_agon.n[[10]])
agon.zoo.samp <- c(by_form_agon.n[[1]], by_form_agon.n[[2]], by_form_agon.n[[3]],
                   by_form_agon.n[[4]], by_form_agon.n[[5]], by_form_agon.n[[6]],
                   by_form_agon.n[[7]], by_form_agon.n[[8]], by_form_agon.n[[9]],
                   by_form_agon.n[[10]])
agon.samp <- as.data.frame(cbind(agon.time, agon.col.samp, agon.zoo.samp))
write.csv(agon.samp,
          "./Results/agon.sampling.per.formation.csv",
          row.names = FALSE)

##### discors ----
#check number of zooids NOT colonies:
# by colonies use mean_by_formation_colony
# by zooid us dat_lg_N
col_form_disc = split.data.frame(disc_mean_by_formation_colony,  #by colonies
                                 disc_mean_by_formation_colony$time) #zooids per formation
#just to look
col_form_disc.n = lapply(col_form_disc, function(x){dim(x)[1]})

## by zooids:
by_form_disc = split.data.frame(dat_lg_N_disc, 
                                dat_lg_N_disc$time)
#just to look
by_form_disc.n = lapply(by_form_disc, function(x){dim(x)[1]})
form_data_disc = lapply(by_form_disc, function(x) x[complete.cases(x),])

#make sampling matrix
disc.time <- levels(disc.mat$time)
length(disc.time)
disc.col.samp <- c(col_form_disc.n[[1]], col_form_disc.n[[2]], col_form_disc.n[[3]],
                   col_form_disc.n[[4]], col_form_disc.n[[5]], col_form_disc.n[[6]],
                   col_form_disc.n[[7]], col_form_disc.n[[8]])
disc.zoo.samp <- c(by_form_disc.n[[1]], by_form_disc.n[[2]], by_form_disc.n[[3]],
                   by_form_disc.n[[4]], by_form_disc.n[[5]], by_form_disc.n[[6]],
                   by_form_disc.n[[7]], by_form_disc.n[[8]])
disc.samp <- as.data.frame(cbind(disc.time, disc.col.samp, disc.zoo.samp))
write.csv(disc.samp,
          "./Results/disc.sampling.per.formation.csv",
          row.names = FALSE)

##### intermedia ----
#check number of zooids NOT colonies:
# by colonies use mean_by_formation_colony
# by zooid us dat_lg_N
col_form_int = split.data.frame(int_mean_by_formation_colony,  #by colonies
                                 int_mean_by_formation_colony$time) #zooids per formation
#just to look
col_form_int.n = lapply(col_form_int, function(x){dim(x)[1]})

## by zooids:
by_form_int = split.data.frame(dat_lg_N_int, 
                                dat_lg_N_int$time)
#just to look
by_form_int.n = lapply(by_form_int, function(x){dim(x)[1]})
form_data_int = lapply(by_form_int, function(x) x[complete.cases(x),])

#make sampling matrix
int.time <- levels(int.mat$time)
length(int.time)
int.col.samp <- c(col_form_int.n[[1]], col_form_int.n[[2]], col_form_int.n[[3]],
                   col_form_int.n[[4]])
int.zoo.samp <- c(by_form_int.n[[1]], by_form_int.n[[2]], by_form_int.n[[3]],
                   by_form_int.n[[4]])
int.samp <- as.data.frame(cbind(int.time, int.col.samp, int.zoo.samp))
write.csv(int.samp,
          "./Results/int.sampling.per.formation.csv",
          row.names = FALSE)

##### speculum ----
#check number of zooids NOT colonies:
# by colonies use mean_by_formation_colony
# by zooid us dat_lg_N
col_form_spec = split.data.frame(spec_mean_by_formation_colony,  #by colonies
                                 spec_mean_by_formation_colony$time) #zooids per formation
#just to look
col_form_spec.n = lapply(col_form_spec, function(x){dim(x)[1]})

## by zooids:
by_form_spec = split.data.frame(dat_lg_N_spec, 
                                dat_lg_N_spec$time)
#just to look
by_form_spec.n = lapply(by_form_spec, function(x){dim(x)[1]})
form_data_spec = lapply(by_form_spec, function(x) x[complete.cases(x),])

#make sampling matrix
spec.time <- levels(spec.mat$time)
length(spec.time)
spec.col.samp <- c(col_form_spec.n[[1]], col_form_spec.n[[2]], col_form_spec.n[[3]],
                   col_form_spec.n[[4]], col_form_spec.n[[5]], col_form_spec.n[[6]],
                   col_form_spec.n[[7]])
spec.zoo.samp <- c(by_form_spec.n[[1]], by_form_spec.n[[2]], by_form_spec.n[[3]],
                   by_form_spec.n[[4]], by_form_spec.n[[5]], by_form_spec.n[[6]],
                   by_form_spec.n[[7]])
spec.samp <- as.data.frame(cbind(spec.time, spec.col.samp, spec.zoo.samp))
write.csv(spec.samp,
          "./Results/spec.sampling.per.formation.csv",
          row.names = FALSE)

#### P MATRICES ----
#p.cov is the same and the phen.var

p.cov.agon = lapply(form_data_agon, function (x){ (cov(x[, 6:7]))}) #traits per colony (not variation within colony)
#p.cov.2 = cov(dat_lg_N.2[5:10]) #traits per colony (not variation within colony)
#does not work with NAs

p.cov.disc = lapply(form_data_disc, function (x){ (cov(x[, 6:7]))}) #traits per colony (not variation within colony)

p.cov.int = lapply(form_data_int, function (x){ (cov(x[, 6:7]))}) #traits per colony (not variation within colony)

p.cov.spec = lapply(form_data_spec, function (x){ (cov(x[, 6:7]))}) #traits per colony (not variation within colony)

##### P VARIANCES ----
##Phenotypic variance in traits and eigen vectors
Pmat.agon = p.cov.agon

p.variances.agon = lapply(Pmat.agon, diag)
paste("Trait variances")
head(p.variances.agon)

Pmat.disc = p.cov.disc

p.variances.disc = lapply(Pmat.disc, diag)
paste("Trait variances")
head(p.variances.disc)

Pmat.int = p.cov.int

p.variances.int = lapply(Pmat.int, diag)
paste("Trait variances")
head(p.variances.int)

Pmat.spec = p.cov.spec

p.variances.spec = lapply(Pmat.spec, diag)
paste("Trait variances")
head(p.variances.spec)

##### P EIGEN -----

##### agonistes -----
p.eig_variances.agon = lapply(Pmat.agon, function (x) {eigen(x)$values})
# lapply(Pmat, function (x) {eigen(x)})
paste("Eigenvalue variances")
head(p.eig_variances.agon)

p.eig_percent.agon = lapply(p.eig_variances.agon, function (x) {x/sum(x)})
p.eig_per_mat.agon = do.call(rbind, p.eig_percent.agon)
p.eig_per_mat.agon = data.frame(p.eig_per_mat.agon, rownames(p.eig_per_mat.agon))
p.eig_per.agon = melt(p.eig_per_mat.agon)
p.eig_per.agon$rownames.p.eig_per_mat.agon. <- factor(p.eig_per.agon$rownames.p.eig_per_mat.agon., 
                                                      levels = c("Nukumaru Limestone",
                                                                 "Nukumaru Brown Sand",
                                                                 "Tewkesbury Formation",
                                                                 "Lower Kai-iwi Shellbed",
                                                                 "Lower Castlecliff",
                                                                 "Shakespeare Cliff Basal Sand Shellbed",
                                                                 "Upper Castlecliff Shellbed",
                                                                 "modern"))
P_PC_dist_agon = ggplot(p.eig_per.agon,
                   aes(x = variable, y = value,
                       group = rownames.p.eig_per_mat.agon.,
                       colour = rownames.p.eig_per_mat.agon.)) +
    geom_line(aes(linetype = rownames.p.eig_per_mat.agon.)) +
    geom_point() +
    scale_y_continuous("Principal component rank",
                       limits = c(-.02, 1.02)) +
    plot.theme + 
    theme_linedraw() +
    ggtitle(expression(paste(italic("M. agonistes")))) +
    scale_x_discrete("Principal component rank",
                     labels = c("PC1", "PC2")) +
    scale_color_manual(values = agon.col.form)


P_PC_dist_agon #none negative; none above 1; dim 8 close to 0; could keep 7

ggsave(P_PC_dist_agon, 
       file = "./Results/P.PC.dist.agon.png", 
       width = 14, height = 10, units = "cm") 

##### discors -----
p.eig_variances.disc = lapply(Pmat.disc, function (x) {eigen(x)$values})
# lapply(Pmat, function (x) {eigen(x)})
paste("Eigenvalue variances")
head(p.eig_variances.disc)

p.eig_percent.disc = lapply(p.eig_variances.disc, function (x) {x/sum(x)})
p.eig_per_mat.disc = do.call(rbind, p.eig_percent.disc)
p.eig_per_mat.disc = data.frame(p.eig_per_mat.disc, rownames(p.eig_per_mat.disc))
p.eig_per.disc = melt(p.eig_per_mat.disc)
p.eig_per.disc$rownames.p.eig_per_mat.disc. <- factor(p.eig_per.disc$rownames.p.eig_per_mat.disc., 
                                                      levels = c("Nukumaru Limestone",
                                                                 "Nukumaru Brown Sand",
                                                                 "Tewkesbury Formation",
                                                                 "Lower Kai-iwi Shellbed",
                                                                 "Lower Castlecliff",
                                                                 "Shakespeare Cliff Basal Sand Shellbed",
                                                                 "Upper Castlecliff Shellbed",
                                                                 "modern"))
P_PC_dist_disc = ggplot(p.eig_per.disc,
                        aes(x = variable, y = value,
                            group = rownames.p.eig_per_mat.disc.,
                            colour = rownames.p.eig_per_mat.disc.)) +
    geom_line(aes(linetype = rownames.p.eig_per_mat.disc.)) +
    geom_point() +
    scale_y_continuous("Principal component rank",
                       limits = c(-.02, 1.02)) +
    plot.theme + 
    theme_linedraw() +
    ggtitle(expression(paste(italic("M. discors")))) +
    scale_x_discrete("Principal component rank",
                     labels = c("PC1", "PC2")) +
    scale_color_manual(values = disc.col.form)


P_PC_dist_disc #none negative; none above 1; dim 8 close to 0; could keep 7

ggsave(P_PC_dist_disc, 
       file = "./Results/P.PC.dist.disc.png", 
       width = 14, height = 10, units = "cm") 

##### intermedia -----
p.eig_variances.int = lapply(Pmat.int, function (x) {eigen(x)$values})
# lapply(Pmat, function (x) {eigen(x)})
paste("Eigenvalue variances")
head(p.eig_variances.int)

p.eig_percent.int = lapply(p.eig_variances.int, function (x) {x/sum(x)})
p.eig_per_mat.int = do.call(rbind, p.eig_percent.int)
p.eig_per_mat.int = data.frame(p.eig_per_mat.int, rownames(p.eig_per_mat.int))
p.eig_per.int = melt(p.eig_per_mat.int)
p.eig_per.int$rownames.p.eig_per_mat.int. <- factor(p.eig_per.int$rownames.p.eig_per_mat.int., 
                                                      levels = c("Nukumaru Limestone",
                                                                 "Nukumaru Brown Sand",
                                                                 "Tewkesbury Formation",
                                                                 "Lower Kai-iwi Shellbed",
                                                                 "Lower Castlecliff",
                                                                 "Shakespeare Cliff Basal Sand Shellbed",
                                                                 "Upper Castlecliff Shellbed",
                                                                 "modern"))
P_PC_dist_int = ggplot(p.eig_per.int,
                        aes(x = variable, y = value,
                            group = rownames.p.eig_per_mat.int.,
                            colour = rownames.p.eig_per_mat.int.)) +
    geom_line(aes(linetype = rownames.p.eig_per_mat.int.)) +
    geom_point() +
    scale_y_continuous("Principal component rank",
                       limits = c(-.02, 1.02)) +
    plot.theme + 
    theme_linedraw() +
    ggtitle(expression(paste(italic("M. intermedia")))) +
    scale_x_discrete("Principal component rank",
                     labels = c("PC1", "PC2")) +
    scale_color_manual(values = int.col.form)


P_PC_dist_int #none negative; none above 1; dim 8 close to 0; could keep 7

ggsave(P_PC_dist_int, 
       file = "./Results/P.PC.dist.int.png", 
       width = 14, height = 10, units = "cm") 

##### speculum -----
p.eig_variances.spec = lapply(Pmat.spec, function (x) {eigen(x)$values})
# lapply(Pmat, function (x) {eigen(x)})
paste("Eigenvalue variances")
head(p.eig_variances.spec)

p.eig_percent.spec = lapply(p.eig_variances.spec, function (x) {x/sum(x)})
p.eig_per_mat.spec = do.call(rbind, p.eig_percent.spec)
p.eig_per_mat.spec = data.frame(p.eig_per_mat.spec, rownames(p.eig_per_mat.spec))
p.eig_per.spec = melt(p.eig_per_mat.spec)
p.eig_per.spec$rownames.p.eig_per_mat.spec. <- factor(p.eig_per.spec$rownames.p.eig_per_mat.spec., 
                                                      levels = c("Nukumaru Limestone",
                                                                 "Nukumaru Brown Sand",
                                                                 "Tewkesbury Formation",
                                                                 "Lower Kai-iwi Shellbed",
                                                                 "Lower Castlecliff",
                                                                 "Shakespeare Cliff Basal Sand Shellbed",
                                                                 "Upper Castlecliff Shellbed",
                                                                 "modern"))
P_PC_dist_spec = ggplot(p.eig_per.spec,
                        aes(x = variable, y = value,
                            group = rownames.p.eig_per_mat.spec.,
                            colour = rownames.p.eig_per_mat.spec.)) +
    geom_line(aes(linetype = rownames.p.eig_per_mat.spec.)) +
    geom_point() +
    scale_y_continuous("Principal component rank",
                       limits = c(-.02, 1.02)) +
    plot.theme + 
    theme_linedraw() +
    ggtitle(expression(paste(italic("M. speculum")))) +
    scale_x_discrete("Principal component rank",
                     labels = c("PC1", "PC2")) +
    scale_color_manual(values = spec.col.form)


P_PC_dist_spec #none negative; none above 1; dim 8 close to 0; could keep 7

ggsave(P_PC_dist_spec, 
       file = "./Results/P.PC.dist.spec.png", 
       width = 14, height = 10, units = "cm") 

##### P CORR -----

p.comp_mat_agon = RandomSkewers(Pmat.agon) #need at least
p.corr_mat_agon = p.comp_mat_agon$correlations + t(p.comp_mat_agon$correlations) 
diag(p.corr_mat_agon) = 1
paste("Random Skewers similarity matrix")
corrplot.mixed(p.corr_mat_agon, upper = "number", lower = "pie")

p.comp_mat_disc = RandomSkewers(Pmat.disc) #need at least
p.corr_mat_disc = p.comp_mat_disc$correlations + t(p.comp_mat_disc$correlations) 
diag(p.corr_mat_disc) = 1
paste("Random Skewers similarity matrix")
corrplot.mixed(p.corr_mat_disc, upper = "number", lower = "pie")

p.comp_mat_int = RandomSkewers(Pmat.int) #need at least
p.corr_mat_int = p.comp_mat_int$correlations + t(p.comp_mat_int$correlations) 
diag(p.corr_mat_int) = 1
paste("Random Skewers similarity matrix")
corrplot.mixed(p.corr_mat_int, upper = "number", lower = "pie")

p.comp_mat_spec = RandomSkewers(Pmat.spec) #need at least
p.corr_mat_spec = p.comp_mat_spec$correlations + t(p.comp_mat_spec$correlations) 
diag(p.corr_mat_spec) = 1
paste("Random Skewers similarity matrix")
corrplot.mixed(p.corr_mat_spec, upper = "number", lower = "pie")

#### G MATRIX ----
#use Sample_ID for locality in modern
#phen.var is same as p.cov

##### AGONISTES -----

###### priors -----
phen.var.agon = lapply(form_data_agon, function (x){ (cov(x[, 6:7]))}) #traits of ALL; correct for colony later
prior.agon = lapply(phen.var.agon, function (x){list(G = list(G1 = list(V = x/2, nu = 2),
                                                              G2 = list(V = x/4,nu = 2)),
                                                     R = list(V = x/4, nu = 2))})

form.mod.agon <- form_data_agon$modern
phen.var.mod.agon = cov(form.mod.agon[, 6:7]) #traits of ALL; correct for colony later
prior.mod.agon = list(G = list(G1 = list(V = phen.var.mod.agon/2, nu = 2),
                               G2 = list(V = phen.var.mod.agon/4, nu = 2),
                               G3 = list(V = phen.var.mod.agon/4, nu = 2)),
                      R = list(V = phen.var.mod.agon/4, nu = 2))

###### mcmc ------
#Running the MCMC chain
model_G_agon = list()
for (i in 1:length(agon.time)){
    model_G_agon[[i]] <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                             #account for variation w/in colony:
                             random = ~us(trait):col.id + us(trait):category, #the number of these determines # of Gs #+ us(trait):formation
                             rcov = ~us(trait):units,
                             family = rep("gaussian", 2), #num of traits
                             data = form_data_agon[[i]],
                             nitt = 1500000, thin = 1000, burnin = 500000,
                             prior = prior.agon[[i]], verbose = TRUE)
}

##need locality data for modern first
model_G_agon.mod <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                        #account for variation w/in colony:
                        random = ~us(trait):col.id + us(trait):category + us(trait):Sample_ID, #the number of these determines # of Gs
                        rcov = ~us(trait):units,
                        family = rep("gaussian", 2), #num of traits
                        data = form.mod.agon, #need to have formation as factor in data matrix
                        nitt = 1500000, thin = 1000, burnin = 500000,
                        prior = prior.mod.agon, verbose = TRUE)

###### save model -----
save(model_G_agon,
     file = "./Results/model_G_agon.RData")

save(model_G_agon.mod,
     file = "./Results/model_G_agon.mod.locality.RData")

model_G_agon.all <- model_G_agon
length(agon.time)
model_G_agon.all[[10]] <- model_G_agon.mod
save(model_G_agon.all,
     file = "./Results/model_G_agon.all.RData")

load(file = "./Results/model_G_agon.all.RData") #load the g matrices calculated above 


##### DISCORS -----

###### priors -----
phen.var.disc = lapply(form_data_disc, function (x){ (cov(x[, 6:7]))}) #traits of ALL; correct for colony later
prior.disc = lapply(phen.var.disc, function (x){list(G = list(G1 = list(V = x/2, nu = 2),
                                                              G2 = list(V = x/4,nu = 2)),
                                                     R = list(V = x/4, nu = 2))})

form.mod.disc <- form_data_disc$modern
phen.var.mod.disc = cov(form.mod.disc[, 6:7]) #traits of ALL; correct for colony later
prior.mod.disc = list(G = list(G1 = list(V = phen.var.mod.disc/2, nu = 2),
                               G2 = list(V = phen.var.mod.disc/4, nu = 2),
                               G3 = list(V = phen.var.mod.disc/4, nu = 2)),
                      R = list(V = phen.var.mod.disc/4, nu = 2))

###### mcmc ------
#Running the MCMC chain
model_G_disc = list()
for (i in 1:length(disc.time)){
    model_G_disc[[i]] <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                                  #account for variation w/in colony:
                                  random = ~us(trait):col.id + us(trait):category, #the number of these determines # of Gs #+ us(trait):formation
                                  rcov = ~us(trait):units,
                                  family = rep("gaussian", 2), #num of traits
                                  data = form_data_disc[[i]],
                                  nitt = 1500000, thin = 1000, burnin = 500000,
                                  prior = prior.disc[[i]], verbose = TRUE)
}

##need locality data for modern first
model_G_disc.mod <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                             #account for variation w/in colony:
                             random = ~us(trait):col.id + us(trait):category + us(trait):Sample_ID, #the number of these determines # of Gs
                             rcov = ~us(trait):units,
                             family = rep("gaussian", 2), #num of traits
                             data = form.mod.disc, #need to have formation as factor in data matrix
                             nitt = 1500000, thin = 1000, burnin = 500000,
                             prior = prior.mod.disc, verbose = TRUE)

###### save model -----
save(model_G_disc,
     file = "./Results/model_G_disc.RData")

save(model_G_disc.mod,
     file = "./Results/model_G_disc.mod.locality.RData")

model_G_disc.all <- model_G_disc
length(disc.time) 
model_G_disc.all[[8]] <- model_G_disc.mod ###CHANGE THIS
save(model_G_disc.all,
     file = "./Results/model_G_disc.all.RData")

load(file = "./Results/model_G_disc.all.RData") #load the g matrices calculated above 

##### INTERMEDIA -----

###### priors -----
phen.var.int = lapply(form_data_int, function (x){ (cov(x[, 6:7]))}) #traits of ALL; correct for colony later
prior.int = lapply(phen.var.int, function (x){list(G = list(G1 = list(V = x/2, nu = 2),
                                                              G2 = list(V = x/4,nu = 2)),
                                                     R = list(V = x/4, nu = 2))})

form.mod.int <- form_data_int$modern
phen.var.mod.int = cov(form.mod.int[, 6:7]) #traits of ALL; correct for colony later
prior.mod.int = list(G = list(G1 = list(V = phen.var.mod.int/2, nu = 2),
                               G2 = list(V = phen.var.mod.int/4, nu = 2),
                               G3 = list(V = phen.var.mod.int/4, nu = 2)),
                      R = list(V = phen.var.mod.int/4, nu = 2))

###### mcmc ------
#Running the MCMC chain
model_G_int = list()
for (i in 1:length(int.time)){
    model_G_int[[i]] <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                                  #account for variation w/in colony:
                                  random = ~us(trait):col.id + us(trait):category, #the number of these determines # of Gs #+ us(trait):formation
                                  rcov = ~us(trait):units,
                                  family = rep("gaussian", 2), #num of traits
                                  data = form_data_int[[i]],
                                  nitt = 1500000, thin = 1000, burnin = 500000,
                                  prior = prior.int[[i]], verbose = TRUE)
}

##need locality data for modern first
model_G_int.mod <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                             #account for variation w/in colony:
                             random = ~us(trait):col.id + us(trait):category + us(trait):Sample_ID, #the number of these determines # of Gs
                             rcov = ~us(trait):units,
                             family = rep("gaussian", 2), #num of traits
                             data = form.mod.int, #need to have formation as factor in data matrix
                             nitt = 1500000, thin = 1000, burnin = 500000,
                             prior = prior.mod.int, verbose = TRUE)

###### save model -----
save(model_G_int,
     file = "./Results/model_G_int.RData")

save(model_G_int.mod,
     file = "./Results/model_G_int.mod.locality.RData")

model_G_int.all <- model_G_int
length(int.time) 
model_G_int.all[[4]] <- model_G_int.mod ###CHANGE THIS
save(model_G_int.all,
     file = "./Results/model_G_int.all.RData")

load(file = "./Results/model_G_int.all.RData") #load the g matrices calculated above 

##### SPECULUM -----

###### priors -----
phen.var.spec = lapply(form_data_spec, function (x){ (cov(x[, 6:7]))}) #traits of ALL; correct for colony later
prior.spec = lapply(phen.var.spec, function (x){list(G = list(G1 = list(V = x/2, nu = 2),
                                                              G2 = list(V = x/4,nu = 2)),
                                                     R = list(V = x/4, nu = 2))})

form.mod.spec <- form_data_spec$modern
phen.var.mod.spec = cov(form.mod.spec[, 6:7]) #traits of ALL; correct for colony later
prior.mod.spec = list(G = list(G1 = list(V = phen.var.mod.spec/2, nu = 2),
                               G2 = list(V = phen.var.mod.spec/4, nu = 2),
                               G3 = list(V = phen.var.mod.spec/4, nu = 2)),
                      R = list(V = phen.var.mod.spec/4, nu = 2))

###### mcmc ------
#Running the MCMC chain
model_G_spec = list()
for (i in 1:length(spec.time)){
    model_G_spec[[i]] <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                                  #account for variation w/in colony:
                                  random = ~us(trait):col.id + us(trait):category, #the number of these determines # of Gs #+ us(trait):formation
                                  rcov = ~us(trait):units,
                                  family = rep("gaussian", 2), #num of traits
                                  data = form_data_spec[[i]],
                                  nitt = 1500000, thin = 1000, burnin = 500000,
                                  prior = prior.spec[[i]], verbose = TRUE)
}

##need locality data for modern first
model_G_spec.mod <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                             #account for variation w/in colony:
                             random = ~us(trait):col.id + us(trait):category + us(trait):Sample_ID, #the number of these determines # of Gs
                             rcov = ~us(trait):units,
                             family = rep("gaussian", 2), #num of traits
                             data = form.mod.spec, #need to have formation as factor in data matrix
                             nitt = 1500000, thin = 1000, burnin = 500000,
                             prior = prior.mod.spec, verbose = TRUE)

###### save model -----
save(model_G_spec,
     file = "./Results/model_G_spec.RData")

save(model_G_spec.mod,
     file = "./Results/model_G_spec.mod.locality.RData")

model_G_spec.all <- model_G_spec
length(spec.time) 
model_G_spec.all[[7]] <- model_G_spec.mod ###CHANGE THIS
save(model_G_spec.all,
     file = "./Results/model_G_spec.all.RData")

load(file = "./Results/model_G_spec.all.RData") #load the g matrices calculated above 



#let's see what the G matrix looks like

phen.var.glob.1 = cov(dat_lg_N.1[, 5:6]) #traits of ALL; correct for colony and formation later
prior.g.1 = list(G = list(G1 = list(V = phen.var.glob.1/2, nu = 2),
                          G2 = list(V = phen.var.glob.1/4,nu = 2)), #nu = 10 #V same as individual G matrices; nu is different
                  R = list(V = phen.var.glob.1/4, nu = 2)) #nu = 5 #V same as individual G matrices

model_G.1 <- MCMCglmm(cbind(ln.len, #same order as in priors
                            ln.wid) ~ trait-1, #get rid of "trait" alone?  + trait:formation
                         #account for variation w/in colony:
                         random = ~us(trait):col.id + us(trait):category, #the number of these determines # of Gs
                         rcov = ~us(trait):units,
                         family = rep("gaussian", 2), #num of traits
                         data = dat_lg_N.1, #need to have formation as factor in data matrix
                         nitt = 150000, thin = 1000, burnin = 50000,
                         prior = prior.g.1, verbose = TRUE)

#check model
summary(model_G.1)
plot(model_G.1$VCV) #catepillar!




#### PLOT TRAITS ----
##### AGONISTES -----
p.zh = ggplot(data = df) + 
    geom_density(aes(x = df[, traits[1]], 
                     group = formation,
                     col = formation)) + 
    plot.theme +
    scale_x_continuous(name = traits[1]) +
    scale_color_manual(values = col.form)

p.mpw.b = ggplot(data = df) + 
    geom_density(aes(x = df[, traits[2]], 
                     group = formation,
                     col = formation)) + 
    plot.theme +
    scale_x_continuous(name = traits[2]) +
    scale_color_manual(values = col.form)

p.cw.m = ggplot(data = df) + 
    geom_density(aes(x = df[, traits[3]], 
                     group = formation,
                     col = formation)) + 
    plot.theme +
    scale_x_continuous(name = traits[3]) +
    scale_color_manual(values = col.form)

p.cw.d = ggplot(data = df) + 
    geom_density(aes(x = df[, traits[4]], 
                     group = formation,
                     col = formation)) + 
    plot.theme +
    scale_x_continuous(name = traits[4]) +
    scale_color_manual(values = col.form)

p.ow.m = ggplot(data = df) + 
    geom_density(aes(x = df[, traits[5]], 
                     group = formation,
                     col = formation)) + 
    plot.theme +
    scale_x_continuous(name = traits[5]) +
    scale_color_manual(values = col.form)

p.oh = ggplot(data = df) + 
    geom_density(aes(x = df[, traits[6]], 
                     group = formation,
                     col = formation)) + 
    plot.theme +
    scale_x_continuous(name = traits[6]) +
    scale_color_manual(values = col.form)

p.c.side = ggplot(data = df) + 
    geom_density(aes(x = df[, traits[7]], 
                     group = formation,
                     col = formation)) + 
    plot.theme +
    scale_x_continuous(name = traits[7]) +
    scale_color_manual(values = col.form)

p.o.side = ggplot(data = df) + 
    geom_density(aes(x = df[, traits[8]], 
                     group = formation,
                     col = formation)) + 
    plot.theme +
    scale_x_continuous(name = traits[8]) +
    scale_color_manual(values = col.form)

Fig = list(p.zh, p.mpw.b, p.cw.m, p.cw.d, p.ow.m, p.oh, p.c.side, p.o.side)
ml <- marrangeGrob(Fig, nrow = 4, ncol = 2)
ml
#see a really big shift between Tewkesbury and earlier and Upper Kai-Iwi and after

ggsave(ml, 
       file = "./Results/trait.distribution.png", 
       width = 14, height = 20, units = "cm")
##### DISCORS -----

##### INTERMEDIA -----

##### SPECULUM -----

#### P MATRIX ----

##### AGONISTES -----
###### EXT P ----

##### DISCORS -----
###### EXT P ----

##### INTERMEDIA -----
###### EXT P ----

##### SPECULUM -----
###### EXT P ----

#### G MATRIX ----

##### AGONISTES -----

##### DISCORS -----

##### INTERMEDIA -----

##### SPECULUM -----

#### G OVER TIME ----

##### AGONISTES -----

##### DISCORS -----

##### INTERMEDIA -----

##### SPECULUM -----

#### ∆z DIRECTIONS OVER TIME ----

##### AGONISTES -----

##### DISCORS -----

##### INTERMEDIA -----

##### SPECULUM -----

#### ANCESTRAL G ----

##### AGONISTES -----

##### DISCORS -----

##### INTERMEDIA -----

##### SPECULUM -----

#### ∆z DIRECTIONS COMPARED TO ANCESTRAL G ----

##### AGONISTES -----

##### DISCORS -----

##### INTERMEDIA -----

##### SPECULUM -----
