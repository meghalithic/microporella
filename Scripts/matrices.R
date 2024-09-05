##1. plot traits
##2. create P matrices
##3. create G matrices
##4. look at matrices over time
##5. compare directions of G and ∆z
##6. create ancestral G matrix (DISCORS)
##7. compare directions of ∆z and ancestral G

#### ENVIRONMENT ----
source("Scripts/env.R")

#### LOAD DATA ----
#this includes all
load(file = "Data/cli.meta.form.list.RData") #load the g matrices calculated above 

agon.meta.form <- cli.meta.form.list[[1]]
disc.meta.form <- cli.meta.form.list[[2]]
int.meta.form <- cli.meta.form.list[[3]]
spec.meta.form <- cli.meta.form.list[[4]]

#this includes minimum three of each zooid type per colony and minimum 3 colonies per formation
load(file = "Data/cli.meta.form.cat.list.RData") #load the g matrices calculated above 

agon.meta.form.cat <- cli.meta.form.cat.list[[1]]
disc.meta.form.cat <- cli.meta.form.cat.list[[2]]
int.meta.form.cat <- cli.meta.form.cat.list[[3]]
spec.meta.form.cat <- cli.meta.form.cat.list[[4]]

#this includes just autozooids with minimum 3 zooids per colony, and minimum 3 colonies per formation
load(file = "Data/cli.meta.form.auto.list.RData") #load the g matrices calculated above 

agon.meta.form.auto <- cli.meta.form.list.auto[[1]]
disc.meta.form.auto <- cli.meta.form.list.auto[[2]]
int.meta.form.auto <- cli.meta.form.list.auto[[3]]
spec.meta.form.auto <- cli.meta.form.list.auto[[4]]

##means
load(file = "Data/mean.list.RData") 

agon_mean_by_formation_colony <- mean.list[[1]]
disc_mean_by_formation_colony <- mean.list[[2]]
int_mean_by_formation_colony <- mean.list[[3]]
spec_mean_by_formation_colony <- mean.list[[4]]
agon_mean_by_formation_colony_cat <- mean.list[[5]]
dic_mean_by_formation_colony_cat <- mean.list[[6]]
int_mean_by_formation_colony_cat <- mean.list[[7]]
spec_mean_by_formation_colony_cat <- mean.list[[8]]

#### MANIPULATE DATA ----
## ONLY AUTOZOOIDS
agon.zooid.list <- unique(agon.meta.form.auto$id)
length(agon.zooid.list) #13486

disc.zooid.list <- unique(disc.meta.form.auto$id)
length(disc.zooid.list) #6448

int.zooid.list <- unique(int.meta.form.auto$id)
length(int.zooid.list) #2678

spec.zooid.list <- unique(spec.meta.form.auto$id)
length(spec.zooid.list) #8376

agon.colony.list <- unique(agon.meta.form.auto$col.id)
length(agon.colony.list) #309

disc.colony.list <- unique(disc.meta.form.auto$col.id)
length(disc.colony.list) #118

int.colony.list <- unique(int.meta.form.auto$col.id)
length(int.colony.list) #73

spec.colony.list <- unique(spec.meta.form.auto$col.id)
length(spec.colony.list) #260

##### SET UP DATASET -----
traits = names(agon.meta.form[, c("ln.len", "ln.wid" )])
length(traits) #2

#cat.keep <- c("autozooid", "avicularium", "ovicell")

#agon.trim <- agon.meta.form.auto[agon.meta.form.auto$category %in% cat.keep,]
#names(agon.trim)

agon.mat <- agon.meta.form.auto %>%
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

#disc.trim <- disc.meta.form[disc.meta.form$category %in% cat.keep,]
#names(disc.trim)

disc.mat <- disc.meta.form.auto %>%
    dplyr::select(time, id, category, col.id, Sample_ID, matches(traits))


#int.trim <- int.meta.form[int.meta.form$category %in% cat.keep,]
#names(int.trim)

int.mat <- int.meta.form.auto %>%
    dplyr::select(time, id, category, col.id, Sample_ID, matches(traits))


#spec.trim <- spec.meta.form[spec.meta.form$category %in% cat.keep,]
#names(spec.trim)

spec.mat <- spec.meta.form.auto %>%
    dplyr::select(time, id, category, col.id, Sample_ID, matches(traits))

agon.mat = as.data.frame(agon.mat)
#agon.mat.2 = as.data.frame(agon.mat.2)
disc.mat = as.data.frame(disc.mat)
int.mat = as.data.frame(int.mat)
spec.mat = as.data.frame(spec.mat)

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
unique(agon.mat$time)
agon.mat$time <- factor(agon.mat$time,
                        levels = c("Nukumaru Limestone",
                                   "Nukumaru Brown Sand",
                                   "Lower Kai-iwi Shellbed",
                                   "Upper Kai-iwi Shellbed",
                                   "Lower Castlecliff Shellbed",
                                   "Tainui Shellbed",
                                   "Shakespeare Cliff Basal Sand Shellbed",
                                   "Whanganui Core",
                                   "modern"))
unique(agon.mat$time)

unique(disc.mat$time)
disc.mat$time <- factor(disc.mat$time,
                        levels = c("Nukumaru Limestone",
                                   "Nukumaru Brown Sand",
                                   "Lower Kai-iwi Shellbed",
                                   "Upper Kai-iwi Shellbed",
                                   "Tainui Shellbed",
                                   "modern"))
unique(disc.mat$time)

unique(int.mat$time)
int.mat$time <- factor(int.mat$time,
                       levels = c("Lower Castlecliff Shellbed",
                                  "Shakespeare Cliff Basal Sand Shellbed",
                                  "Whanganui Core",
                                  "modern"))
unique(int.mat$time)

unique(spec.mat$time)
spec.mat$time <- factor(spec.mat$time,
                        levels = c("Upper Kai-iwi Shellbed",
                                   "Lower Castlecliff Shellbed",
                                   "Tainui Shellbed",
                                   "Shakespeare Cliff Basal Sand Shellbed",
                                   "modern"))
unique(spec.mat$time)

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

# p.agon.avic.len = ggplot(agon.mat[agon.mat$category == "avicularium",]) + 
#     geom_density(aes(x = agon.mat[agon.mat$category == "avicularium", traits[1]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. agonistes")," avicularium"))) + 
#     scale_x_continuous(name = traits[1]) +
#     scale_color_manual(values = agon.col.form)
# 
# p.agon.avic.wid = ggplot(data = agon.mat[agon.mat$category == "avicularium",]) + 
#     geom_density(aes(x = agon.mat[agon.mat$category == "avicularium", traits[2]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. agonistes")," avicularium"))) + 
#     scale_x_continuous(name = traits[2]) +
#     scale_color_manual(values = agon.col.form)
# 
# p.agon.ovi.len = ggplot(agon.mat[agon.mat$category == "ovicell",]) + 
#     geom_density(aes(x = agon.mat[agon.mat$category == "ovicell", traits[1]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. agonistes")," ovicell"))) + 
#     scale_x_continuous(name = traits[1]) +
#     scale_color_manual(values = agon.col.form)
# 
# p.agon.ovi.wid = ggplot(data = agon.mat[agon.mat$category == "ovicell",]) + 
#     geom_density(aes(x = agon.mat[agon.mat$category == "ovicell", traits[2]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. agonistes")," ovicell"))) + 
#     scale_x_continuous(name = traits[2]) +
#     scale_color_manual(values = agon.col.form)
# 
# 
# agon.fig = list(p.agon.auto.len, p.agon.avic.len,
#                 p.agon.ovi.len,
#                 p.agon.auto.wid, p.agon.avic.wid, 
#                 p.agon.ovi.wid)
# agon.ml <- marrangeGrob(agon.fig, nrow = 3, ncol = 2)
#agon.ml
#see a really big shift between Tewkesbury and earlier and Upper Kai-Iwi and after

#ggsave(agon.ml, 
#       file = "./Results/agon.trait.distribution.png", 
#       width = 14, height = 20, units = "cm")

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

# p.disc.avic.len = ggplot(disc.mat[disc.mat$category == "avicularium",]) + 
#     geom_density(aes(x = disc.mat[disc.mat$category == "avicularium", traits[1]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. discors")," avicularium"))) + 
#     scale_x_continuous(name = traits[1]) +
#     scale_color_manual(values = disc.col.form)
# 
# p.disc.avic.wid = ggplot(data = disc.mat[disc.mat$category == "avicularium",]) + 
#     geom_density(aes(x = disc.mat[disc.mat$category == "avicularium", traits[2]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. discors")," avicularium"))) + 
#     scale_x_continuous(name = traits[2]) +
#     scale_color_manual(values = disc.col.form)
# 
# p.disc.ovi.len = ggplot(disc.mat[disc.mat$category == "ovicell",]) + 
#     geom_density(aes(x = disc.mat[disc.mat$category == "ovicell", traits[1]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. discors")," ovicell"))) + 
#     scale_x_continuous(name = traits[1]) +
#     scale_color_manual(values = disc.col.form)
# 
# p.disc.ovi.wid = ggplot(data = disc.mat[disc.mat$category == "ovicell",]) + 
#     geom_density(aes(x = disc.mat[disc.mat$category == "ovicell", traits[2]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. discors")," ovicell"))) + 
#     scale_x_continuous(name = traits[2]) +
#     scale_color_manual(values = disc.col.form)
# 
# 
# disc.fig = list(p.disc.auto.len, p.disc.avic.len,
#                 p.disc.ovi.len,
#                 p.disc.auto.wid, p.disc.avic.wid, 
#                 p.disc.ovi.wid)
# disc.ml <- marrangeGrob(disc.fig, nrow = 3, ncol = 2)
# disc.ml
# #see a really big shift between Tewkesbury and earlier and Upper Kai-Iwi and after
# 
# ggsave(disc.ml, 
#        file = "./Results/disc.trait.distribution.png", 
#        width = 14, height = 20, units = "cm")

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

# p.int.avic.len = ggplot(int.mat[int.mat$category == "avicularium",]) + 
#     geom_density(aes(x = int.mat[int.mat$category == "avicularium", traits[1]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. intermedia")," avicularium"))) + 
#     scale_x_continuous(name = traits[1]) +
#     scale_color_manual(values = int.col.form)
# 
# p.int.avic.wid = ggplot(data = int.mat[int.mat$category == "avicularium",]) + 
#     geom_density(aes(x = int.mat[int.mat$category == "avicularium", traits[2]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. intermedia")," avicularium"))) + 
#     scale_x_continuous(name = traits[2]) +
#     scale_color_manual(values = int.col.form)
# 
# p.int.ovi.len = ggplot(int.mat[int.mat$category == "ovicell",]) + 
#     geom_density(aes(x = int.mat[int.mat$category == "ovicell", traits[1]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. intermedia")," ovicell"))) + 
#     scale_x_continuous(name = traits[1]) +
#     scale_color_manual(values = int.col.form)
# 
# p.int.ovi.wid = ggplot(data = int.mat[int.mat$category == "ovicell",]) + 
#     geom_density(aes(x = int.mat[int.mat$category == "ovicell", traits[2]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. intermedia")," ovicell"))) + 
#     scale_x_continuous(name = traits[2]) +
#     scale_color_manual(values = int.col.form)
# 
# int.fig = list(p.int.auto.len, p.int.avic.len,
#                 p.int.ovi.len,
#                 p.int.auto.wid, p.int.avic.wid, 
#                 p.int.ovi.wid)
# int.ml <- marrangeGrob(int.fig, nrow = 3, ncol = 2)
# int.ml
# #see a really big shift between Tewkesbury and earlier and Upper Kai-Iwi and after
# 
# ggsave(int.ml, 
#        file = "./Results/int.trait.distribution.png", 
#        width = 14, height = 20, units = "cm")

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

# p.spec.avic.len = ggplot(spec.mat[spec.mat$category == "avicularium",]) + 
#     geom_density(aes(x = spec.mat[spec.mat$category == "avicularium", traits[1]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. speculum")," avicularium"))) + 
#     scale_x_continuous(name = traits[1]) +
#     scale_color_manual(values = spec.col.form)
# 
# p.spec.avic.wid = ggplot(data = spec.mat[spec.mat$category == "avicularium",]) + 
#     geom_density(aes(x = spec.mat[spec.mat$category == "avicularium", traits[2]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. speculum")," avicularium"))) + 
#     scale_x_continuous(name = traits[2]) +
#     scale_color_manual(values = spec.col.form)
# 
# p.spec.ovi.len = ggplot(spec.mat[spec.mat$category == "ovicell",]) + 
#     geom_density(aes(x = spec.mat[spec.mat$category == "ovicell", traits[1]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. speculum")," ovicell"))) + 
#     scale_x_continuous(name = traits[1]) +
#     scale_color_manual(values = spec.col.form)
# 
# p.spec.ovi.wid = ggplot(data = spec.mat[spec.mat$category == "ovicell",]) + 
#     geom_density(aes(x = spec.mat[spec.mat$category == "ovicell", traits[2]], 
#                      group = time,
#                      col = time)) + 
#     plot.theme +
#     ggtitle(expression(paste(italic("M. speculum")," ovicell"))) + 
#     scale_x_continuous(name = traits[2]) +
#     scale_color_manual(values = spec.col.form)
# 
# 
# spec.fig = list(p.spec.auto.len, p.spec.avic.len,
#                p.spec.ovi.len,
#                p.spec.auto.wid, p.spec.avic.wid, 
#                p.spec.ovi.wid)
# spec.ml <- marrangeGrob(spec.fig, nrow = 3, ncol = 2)
# spec.ml
# #see a really big shift between Tewkesbury and earlier and Upper Kai-Iwi and after
# 
# ggsave(spec.ml, 
#        file = "./Results/spec.trait.distribution.png", 
#        width = 14, height = 20, units = "cm")

##### FULL FIGURE -----
fig = list(p.agon.auto.len, p.disc.auto.len,
           p.int.auto.len, p.spec.auto.len,
           p.agon.auto.wid, p.disc.auto.wid,
           p.int.auto.wid, p.spec.auto.wid)
ml <- marrangeGrob(fig, nrow = 4, ncol = 2)
ml

ggsave(ml,
       file = "./Results/auto.trait.distribution.png", 
       width = 14, height = 20, units = "cm")
       
       
#### SPLIT BY FORMATION ----

##### agonistes ----
#check number of zooids NOT colonies:
# by colonies use mean_by_formation_colony
# by zooid us dat_lg_N
col_form_agon = split.data.frame(agon_mean_by_formation_colony_auto,  #by colonies
                                 agon_mean_by_formation_colony_auto$time) #zooids per formation
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
                   col_form_agon.n[[7]], col_form_agon.n[[8]], col_form_agon.n[[9]])
agon.zoo.samp <- c(by_form_agon.n[[1]], by_form_agon.n[[2]], by_form_agon.n[[3]],
                   by_form_agon.n[[4]], by_form_agon.n[[5]], by_form_agon.n[[6]],
                   by_form_agon.n[[7]], by_form_agon.n[[8]], by_form_agon.n[[9]])
agon.samp <- as.data.frame(cbind(agon.time, agon.col.samp, agon.zoo.samp))
write.csv(agon.samp,
          "./Results/agon.sampling.per.formation.csv",
          row.names = FALSE)

##### discors ----
#check number of zooids NOT colonies:
# by colonies use mean_by_formation_colony
# by zooid us dat_lg_N
col_form_disc = split.data.frame(disc_mean_by_formation_colony_auto,  #by colonies
                                 disc_mean_by_formation_colony_auto$time) #zooids per formation
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
                   col_form_disc.n[[4]], col_form_disc.n[[5]], col_form_disc.n[[6]])
disc.zoo.samp <- c(by_form_disc.n[[1]], by_form_disc.n[[2]], by_form_disc.n[[3]],
                   by_form_disc.n[[4]], by_form_disc.n[[5]], by_form_disc.n[[6]])
disc.samp <- as.data.frame(cbind(disc.time, disc.col.samp, disc.zoo.samp))
write.csv(disc.samp,
          "./Results/disc.sampling.per.formation.csv",
          row.names = FALSE)

##### intermedia ----
#check number of zooids NOT colonies:
# by colonies use mean_by_formation_colony
# by zooid us dat_lg_N
col_form_int = split.data.frame(int_mean_by_formation_colony_auto,  #by colonies
                                int_mean_by_formation_colony_auto$time) #zooids per formation
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
int.col.samp <- c(col_form_int.n[[1]], col_form_int.n[[2]], col_form_int.n[[3]])
int.zoo.samp <- c(by_form_int.n[[1]], by_form_int.n[[2]], by_form_int.n[[3]])
int.samp <- as.data.frame(cbind(int.time, int.col.samp, int.zoo.samp))
write.csv(int.samp,
          "./Results/int.sampling.per.formation.csv",
          row.names = FALSE)

##### speculum ----
#check number of zooids NOT colonies:
# by colonies use mean_by_formation_colony
# by zooid us dat_lg_N
col_form_spec = split.data.frame(spec_mean_by_formation_colony_auto,  #by colonies
                                 spec_mean_by_formation_colony_auto$time) #zooids per formation
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
                   col_form_spec.n[[4]], col_form_spec.n[[5]])
spec.zoo.samp <- c(by_form_spec.n[[1]], by_form_spec.n[[2]], by_form_spec.n[[3]],
                   by_form_spec.n[[4]], by_form_spec.n[[5]])
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
                                                                 "Lower Kai-iwi Shellbed",
                                                                 "Upper Kai-iwi Shellbed",
                                                                 "Lower Castlecliff Shellbed",
                                                                 "Tainui Shellbed",
                                                                 "Shakespeare Cliff Basal Sand Shellbed",
                                                                 "Whanganui Core",
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
       file = "./Results/P.PC.dist.agon.auto.png", 
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
                                                                 "Lower Kai-iwi Shellbed",
                                                                 "Upper Kai-iwi Shellbed",
                                                                 "Tainui Shellbed",
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
       file = "./Results/P.PC.dist.disc.auto.png", 
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
                                                      levels = c("Lower Castlecliff Shellbed",
                                                                 "Whanganui Core",
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
       file = "./Results/P.PC.dist.int.auto.png", 
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
                                                      levels = c("Upper Kai-iwi Shellbed",
                                                                 "Lower Castlecliff Shellbed",
                                                                 "Tainui Shellbed",
                                                                 "Shakespeare Cliff Basal Sand Shellbed",
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
       file = "./Results/P.PC.dist.spec.auto.png", 
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
# prior.agon = lapply(phen.var.agon, function (x){list(G = list(G1 = list(V = x/2, nu = 2),
#                                                               G2 = list(V = x/4,nu = 2)),
#                                                      R = list(V = x/4, nu = 2))})

prior.agon = lapply(phen.var.agon, function (x){list(G = list(G1 = list(V = x/2, nu = 2)),
                                                     R = list(V = x/4, nu = 2))})

form.mod.agon <- form_data_agon$modern
phen.var.mod.agon = cov(form.mod.agon[, 6:7]) #traits of ALL; correct for colony later
# prior.mod.agon = list(G = list(G1 = list(V = phen.var.mod.agon/2, nu = 2),
#                                G2 = list(V = phen.var.mod.agon/4, nu = 2),
#                                G3 = list(V = phen.var.mod.agon/4, nu = 2)),
#                       R = list(V = phen.var.mod.agon/4, nu = 2))

prior.mod.agon = list(G = list(G1 = list(V = phen.var.mod.agon/2, nu = 2),
                               G2 = list(V = phen.var.mod.agon/4, nu = 2)),
                      R = list(V = phen.var.mod.agon/4, nu = 2))

###### mcmc ------
#Running the MCMC chain
model_G_agon = list()
for (i in 1:length(agon.time)){
    model_G_agon[[i]] <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                             #account for variation w/in colony:
                             random = ~us(trait):col.id, #+ us(trait):category, #the number of these determines # of Gs #+ us(trait):formation
                             rcov = ~us(trait):units,
                             family = rep("gaussian", 2), #num of traits
                             data = form_data_agon[[i]],
                             nitt = 1500000, thin = 1000, burnin = 500000,
                             prior = prior.agon[[i]], verbose = TRUE)
}

##need locality data for modern first
model_G_agon.mod <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                        #account for variation w/in colony:
                        random = ~us(trait):col.id + us(trait):Sample_ID, #+ us(trait):category, #the number of these determines # of Gs
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
form_data_agon #3rd is modern
model_G_agon.all[[3]] <- model_G_agon.mod
save(model_G_agon.all,
     file = "./Results/model_G_agon.all.RData")

load(file = "./Results/model_G_agon.all.RData") #load the g matrices calculated above 


##### DISCORS -----

###### priors -----
phen.var.disc = lapply(form_data_disc, function (x){ (cov(x[, 6:7]))}) #traits of ALL; correct for colony later
# prior.disc = lapply(phen.var.disc, function (x){list(G = list(G1 = list(V = x/2, nu = 2),
#                                                               G2 = list(V = x/4,nu = 2)),
#                                                      R = list(V = x/4, nu = 2))})

prior.disc = lapply(phen.var.disc, function (x){list(G = list(G1 = list(V = x/2, nu = 2)),
                                                     R = list(V = x/4, nu = 2))})

form.mod.disc <- form_data_disc$modern
phen.var.mod.disc = cov(form.mod.disc[, 6:7]) #traits of ALL; correct for colony later
# prior.mod.disc = list(G = list(G1 = list(V = phen.var.mod.disc/2, nu = 2),
#                                G2 = list(V = phen.var.mod.disc/4, nu = 2),
#                                G3 = list(V = phen.var.mod.disc/4, nu = 2)),
#                       R = list(V = phen.var.mod.disc/4, nu = 2))

prior.mod.disc = list(G = list(G1 = list(V = phen.var.mod.disc/2, nu = 2),
                               G2 = list(V = phen.var.mod.disc/4, nu = 2)),
                      R = list(V = phen.var.mod.disc/4, nu = 2))

###### mcmc ------
#Running the MCMC chain
model_G_disc = list()
for (i in 1:length(disc.time)){
    model_G_disc[[i]] <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                                  #account for variation w/in colony:
                                  random = ~us(trait):col.id, #+ us(trait):category, #the number of these determines # of Gs #+ us(trait):formation
                                  rcov = ~us(trait):units,
                                  family = rep("gaussian", 2), #num of traits
                                  data = form_data_disc[[i]],
                                  nitt = 1500000, thin = 1000, burnin = 500000,
                                  prior = prior.disc[[i]], verbose = TRUE)
}

##need locality data for modern first
model_G_disc.mod <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                             #account for variation w/in colony:
                             random = ~us(trait):col.id + us(trait):Sample_ID, #us(trait):category + #the number of these determines # of Gs
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
form_data_disc #2 is modern
model_G_disc.all[[2]] <- model_G_disc.mod ###CHANGE THIS
save(model_G_disc.all,
     file = "./Results/model_G_disc.all.RData")

load(file = "./Results/model_G_disc.all.RData") #load the g matrices calculated above 

##### INTERMEDIA -----

###### priors -----
phen.var.int = lapply(form_data_int, function (x){ (cov(x[, 6:7]))}) #traits of ALL; correct for colony later
# prior.int = lapply(phen.var.int, function (x){list(G = list(G1 = list(V = x/2, nu = 2),
#                                                             G2 = list(V = x/4,nu = 2)),
#                                                    R = list(V = x/4, nu = 2))})

prior.int = lapply(phen.var.int, function (x){list(G = list(G1 = list(V = x/2, nu = 2)),
                                                   R = list(V = x/4, nu = 2))})

form.mod.int <- form_data_int$modern
phen.var.mod.int = cov(form.mod.int[, 6:7]) #traits of ALL; correct for colony later
# prior.mod.int = list(G = list(G1 = list(V = phen.var.mod.int/2, nu = 2),
#                               G2 = list(V = phen.var.mod.int/4, nu = 2),
#                               G3 = list(V = phen.var.mod.int/4, nu = 2)),
#                      R = list(V = phen.var.mod.int/4, nu = 2))

prior.mod.int = list(G = list(G1 = list(V = phen.var.mod.int/2, nu = 2),
                              G2 = list(V = phen.var.mod.int/4, nu = 2)),
                     R = list(V = phen.var.mod.int/4, nu = 2))

###### mcmc ------
#Running the MCMC chain
model_G_int = list()
for (i in 1:length(int.time)){
    model_G_int[[i]] <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                                  #account for variation w/in colony:
                                  random = ~us(trait):col.id, #+ us(trait):category, #the number of these determines # of Gs #+ us(trait):formation
                                  rcov = ~us(trait):units,
                                  family = rep("gaussian", 2), #num of traits
                                  data = form_data_int[[i]],
                                  nitt = 1500000, thin = 1000, burnin = 500000,
                                  prior = prior.int[[i]], verbose = TRUE)
}

##need locality data for modern first
model_G_int.mod <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                             #account for variation w/in colony:
                             random = ~us(trait):col.id + us(trait):Sample_ID, #+ us(trait):category #the number of these determines # of Gs
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
form_data_int #2 is modern
model_G_int.all[[2]] <- model_G_int.mod 
save(model_G_int.all,
     file = "./Results/model_G_int.all.RData")

load(file = "./Results/model_G_int.all.RData") #load the g matrices calculated above 

##### SPECULUM -----

###### priors -----
phen.var.spec = lapply(form_data_spec, function (x){ (cov(x[, 6:7]))}) #traits of ALL; correct for colony later
# prior.spec = lapply(phen.var.spec, function (x){list(G = list(G1 = list(V = x/2, nu = 2),
#                                                               G2 = list(V = x/4,nu = 2)),
#                                                      R = list(V = x/4, nu = 2))})

prior.spec = lapply(phen.var.spec, function (x){list(G = list(G1 = list(V = x/2, nu = 2)),
                                                     R = list(V = x/4, nu = 2))})

form.mod.spec <- form_data_spec$modern
phen.var.mod.spec = cov(form.mod.spec[, 6:7]) #traits of ALL; correct for colony later
# prior.mod.spec = list(G = list(G1 = list(V = phen.var.mod.spec/2, nu = 2),
#                                G2 = list(V = phen.var.mod.spec/2, nu = 2),
#                                G3 = list(V = phen.var.mod.spec/4, nu = 2)),
#                       R = list(V = phen.var.mod.spec/4, nu = 2))

prior.mod.spec = list(G = list(G1 = list(V = phen.var.mod.spec/2, nu = 2),
                               G2 = list(V = phen.var.mod.spec/2, nu = 2)),
                      R = list(V = phen.var.mod.spec/4, nu = 2))

###### mcmc ------
#Running the MCMC chain
model_G_spec = list()
for (i in 1:length(spec.time)){
    model_G_spec[[i]] <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                                  #account for variation w/in colony:
                                  random = ~us(trait):col.id, #+ us(trait):category, #the number of these determines # of Gs #+ us(trait):formation
                                  rcov = ~us(trait):units,
                                  family = rep("gaussian", 2), #num of traits
                                  data = form_data_spec[[i]],
                                  nitt = 1500000, thin = 1000, burnin = 500000,
                                  prior = prior.spec[[i]], verbose = TRUE)
}

##need locality data for modern first
model_G_spec.mod <- MCMCglmm(cbind(ln.len, ln.wid) ~ trait-1, #same order as in priors
                             #account for variation w/in colony:
                             random = ~us(trait):col.id + us(trait):Sample_ID, #+ us(trait):category #the number of these determines # of Gs
                             rcov = ~us(trait):units,
                             family = rep("gaussian", 2), #num of traits
                             data = form.mod.spec, #need to have formation as factor in data matrix
                             nitt = 1500000, thin = 1000, burnin = 500000,
                             prior = prior.mod.spec, verbose = TRUE)
#Error in MCMCglmm(cbind(ln.len, ln.wid) ~ trait - 1, random = ~us(trait):col.id + : 
#Mixed model equations singular: use a (stronger) prior

###### save model -----
save(model_G_spec,
     file = "./Results/model_G_spec.RData")

save(model_G_spec.mod,
     file = "./Results/model_G_spec.mod.locality.RData")

model_G_spec.all <- model_G_spec
length(spec.time) 
form_data_spec #2 is modern
model_G_spec.all[[2]] <- model_G_spec.mod ###CHANGE THIS
save(model_G_spec.all,
     file = "./Results/model_G_spec.all.RData")

load(file = "./Results/model_G_spec.all.RData") #load the g matrices calculated above 

##### CHECK MODELS -----
ntraits = 2

###### agonistes ------
summary(model_G_agon.all[[1]])
summary(model_G_agon.all[[2]])
summary(model_G_agon.all[[3]])
summary(model_G_agon.all[[4]])
summary(model_G_agon.all[[5]])
summary(model_G_agon.all[[6]])
summary(model_G_agon.all[[7]])
summary(model_G_agon.all[[8]])
summary(model_G_agon.all[[9]])

##plots to see where sampling from:
plot(model_G_agon.all[[1]]$VCV) #catepillar!
plot(model_G_agon.all[[2]]$VCV) #catepillar!
plot(model_G_agon.all[[3]]$VCV) #catepillar!
plot(model_G_agon.all[[4]]$VCV) #catepillar!
plot(model_G_agon.all[[5]]$VCV) #catepillar!
plot(model_G_agon.all[[6]]$VCV) #catepillar!
plot(model_G_agon.all[[7]]$VCV) #catepillar!
plot(model_G_agon.all[[8]]$VCV) #catepillar!
plot(model_G_agon.all[[9]]$VCV) #catepillar!

g.model.agon = model_G_agon.all
Gmat.agon = lapply(g.model.agon, function (x) { 
    matrix(posterior.mode(x$VCV)[1:ntraits^2], ntraits, ntraits)})
#label lists as formations
names(Gmat.agon) = names(by_form_agon) #formation_list or form_data
#traits in Gmat are in different order than Pmat based on VCV 

# why aren't traits labeled??
for (i in seq_along(Gmat.agon)){
    colnames(Gmat.agon[[i]]) <- traits
}
for (i in seq_along(Gmat.agon)){
    rownames(Gmat.agon[[i]]) <- traits
}

###### discors ------
summary(model_G_disc.all[[1]])
summary(model_G_disc.all[[2]])
summary(model_G_disc.all[[3]])
summary(model_G_disc.all[[4]])
summary(model_G_disc.all[[5]])
summary(model_G_disc.all[[6]])

##plots to see where sampling from:
plot(model_G_disc.all[[1]]$VCV) #catepillar!
plot(model_G_disc.all[[2]]$VCV) #catepillar!
plot(model_G_disc.all[[3]]$VCV) #catepillar!
plot(model_G_disc.all[[4]]$VCV) #catepillar! #not much variation
plot(model_G_disc.all[[5]]$VCV) #catepillar!
plot(model_G_disc.all[[6]]$VCV) #catepillar!

g.model.disc = model_G_disc.all
Gmat.disc = lapply(g.model.disc, function (x) { 
    matrix(posterior.mode(x$VCV)[1:ntraits^2], ntraits, ntraits)})
#label lists as formations
names(Gmat.disc) = names(by_form_disc) #formation_list or form_data
#traits in Gmat are in different order than Pmat based on VCV 

# why aren't traits labeled??
for (i in seq_along(Gmat.disc)){
    colnames(Gmat.disc[[i]]) <- traits
}
for (i in seq_along(Gmat.disc)){
    rownames(Gmat.disc[[i]]) <- traits
}

###### intermedia ------
summary(model_G_int.all[[1]])
summary(model_G_int.all[[2]])
summary(model_G_int.all[[3]])

##plots to see where sampling from:
plot(model_G_int.all[[1]]$VCV) #catepillar! #not much variation
plot(model_G_int.all[[2]]$VCV) #catepillar!
plot(model_G_int.all[[3]]$VCV) #catepillar!

g.model.int = model_G_int.all
Gmat.int = lapply(g.model.int, function (x) { 
    matrix(posterior.mode(x$VCV)[1:ntraits^2], ntraits, ntraits)})
#label lists as formations
names(Gmat.int) = names(by_form_int) #formation_list or form_data
#traits in Gmat are in different order than Pmat based on VCV 

# why aren't traits labeled??
for (i in seq_along(Gmat.int)){
    colnames(Gmat.int[[i]]) <- traits
}
for (i in seq_along(Gmat.int)){
    rownames(Gmat.int[[i]]) <- traits
}

###### speculum ------
summary(model_G_spec.all[[1]])
summary(model_G_spec.all[[2]])
summary(model_G_spec.all[[3]])
summary(model_G_spec.all[[4]])
summary(model_G_spec.all[[5]])

##plots to see where sampling from:
plot(model_G_spec.all[[1]]$VCV) #catepillar! weird, not a lot of variation
plot(model_G_spec.all[[2]]$VCV) #catepillar!
plot(model_G_spec.all[[3]]$VCV) #catepillar!
plot(model_G_spec.all[[4]]$VCV) #catepillar!
plot(model_G_spec.all[[5]]$VCV) #catepillar!

g.model.spec = model_G_spec.all
Gmat.spec = lapply(g.model.spec, function (x) { 
    matrix(posterior.mode(x$VCV)[1:ntraits^2], ntraits, ntraits)})
#label lists as formations
names(Gmat.spec) = names(by_form_spec) #formation_list or form_data
#traits in Gmat are in different order than Pmat based on VCV 

# why aren't traits labeled??
for (i in seq_along(Gmat.spec)){
    colnames(Gmat.spec[[i]]) <- traits
}
for (i in seq_along(Gmat.spec)){
    rownames(Gmat.spec[[i]]) <- traits
}

##### G EIGEN -----
###### agonistes ------
lapply(Gmat.agon, isSymmetric)  #is.symmetric.matrix
g.variances.agon = lapply(Gmat.agon, diag)
paste("Trait variances")
head(g.variances.agon)
g.variances.agon

g.eig_variances.agon = lapply(Gmat.agon, function (x) {eigen(x)$values})
paste("Eigenvalue variances")
head(g.eig_variances.agon)

g.eig_percent.agon = lapply(g.eig_variances.agon, function (x) {x/sum(x)})
g.eig_per_mat.agon = do.call(rbind, g.eig_percent.agon)
g.eig_per_mat.agon = data.frame(g.eig_per_mat.agon, rownames(g.eig_per_mat.agon))
g.eig_per.agon = melt(g.eig_per_mat.agon)
g.eig_per.agon$rownames.g.eig_per_mat.agon. <- factor(g.eig_per.agon$rownames.g.eig_per_mat.agon., 
                                            levels = c("Nukumaru Limestone",
                                                       "Nukumaru Brown Sand",
                                                       "Lower Kai-iwi Shellbed",
                                                       "Upper Kai-iwi Shellbed",
                                                       "Lower Castlecliff Shellbed",
                                                       "Tainui Shellbed",
                                                       "Shakespeare Cliff Basal Sand Shellbed",
                                                       "Whanganui Core",
                                                       "modern"))

G_PC_dist.agon = ggplot(g.eig_per.agon,
                        aes(x = variable, y = value,
                            group = rownames.g.eig_per_mat.agon.,
                            colour = rownames.g.eig_per_mat.agon.)) +
    geom_line(aes(linetype = rownames.g.eig_per_mat.agon.)) +
    geom_point() +
    plot.theme + 
    theme_linedraw() +
    scale_y_continuous("Principal component rank",
                       limits = c(-.02, 1.02)) +
    plot.theme + 
    theme_linedraw() +
    ggtitle(expression(paste(italic("M. agonistes")))) +
    scale_x_discrete("Principal component rank",
                     labels = c("PC1", "PC2")) +
    scale_color_manual(values = agon.col.form)

G_PC_dist.agon 

ggsave(G_PC_dist.agon, 
       file = "./Results/G.PC.dist.agon.png", 
       width = 14, height = 10, units = "cm")

###### discors ------
lapply(Gmat.disc, isSymmetric)  #is.symmetric.matrix
g.variances.disc = lapply(Gmat.disc, diag)
paste("Trait variances")
head(g.variances.disc)
g.variances.disc

g.eig_variances.disc = lapply(Gmat.disc, function (x) {eigen(x)$values})
paste("Eigenvalue variances")
head(g.eig_variances.disc)

g.eig_percent.disc = lapply(g.eig_variances.disc, function (x) {x/sum(x)})
g.eig_per_mat.disc = do.call(rbind, g.eig_percent.disc)
g.eig_per_mat.disc = data.frame(g.eig_per_mat.disc, rownames(g.eig_per_mat.disc))
g.eig_per.disc = melt(g.eig_per_mat.disc)
g.eig_per.disc$rownames.g.eig_per_mat.disc. <- factor(g.eig_per.disc$rownames.g.eig_per_mat.disc., 
                                                      levels = c("Nukumaru Limestone",
                                                                 "Nukumaru Brown Sand",
                                                                 "Lower Kai-iwi Shellbed",
                                                                 "Upper Kai-iwi Shellbed",
                                                                 "Tainui Shellbed",
                                                                 "modern"))

G_PC_dist.disc = ggplot(g.eig_per.disc,
                        aes(x = variable, y = value,
                            group = rownames.g.eig_per_mat.disc.,
                            colour = rownames.g.eig_per_mat.disc.)) +
    geom_line(aes(linetype = rownames.g.eig_per_mat.disc.)) +
    geom_point() +
    plot.theme + 
    theme_linedraw() +
    scale_y_continuous("Principal component rank",
                       limits = c(-.02, 1.02)) +
    plot.theme + 
    theme_linedraw() +
    ggtitle(expression(paste(italic("M. discors")))) +
    scale_x_discrete("Principal component rank",
                     labels = c("PC1", "PC2")) +
    scale_color_manual(values = agon.col.form)

G_PC_dist.disc 

ggsave(G_PC_dist.disc, 
       file = "./Results/G.PC.dist.disc.png", 
       width = 14, height = 10, units = "cm")

###### intermedia ------
lapply(Gmat.int, isSymmetric)  #is.symmetric.matrix
g.variances.int = lapply(Gmat.int, diag)
paste("Trait variances")
head(g.variances.int)
g.variances.int

g.eig_variances.int = lapply(Gmat.int, function (x) {eigen(x)$values})
paste("Eigenvalue variances")
head(g.eig_variances.int)

g.eig_percent.int = lapply(g.eig_variances.int, function (x) {x/sum(x)})
g.eig_per_mat.int = do.call(rbind, g.eig_percent.int)
g.eig_per_mat.int = data.frame(g.eig_per_mat.int, rownames(g.eig_per_mat.int))
g.eig_per.int = melt(g.eig_per_mat.int)
g.eig_per.int$rownames.g.eig_per_mat.int. <- factor(g.eig_per.int$rownames.g.eig_per_mat.int., 
                                                    levels = c("Lower Castlecliff Shellbed",
                                                               "Whanganui Core",
                                                               "modern"))

G_PC_dist.int = ggplot(g.eig_per.int,
                        aes(x = variable, y = value,
                            group = rownames.g.eig_per_mat.int.,
                            colour = rownames.g.eig_per_mat.int.)) +
    geom_line(aes(linetype = rownames.g.eig_per_mat.int.)) +
    geom_point() +
    plot.theme + 
    theme_linedraw() +
    scale_y_continuous("Principal component rank",
                       limits = c(-.02, 1.02)) +
    plot.theme + 
    theme_linedraw() +
    ggtitle(expression(paste(italic("M. intermedia")))) +
    scale_x_discrete("Principal component rank",
                     labels = c("PC1", "PC2")) +
    scale_color_manual(values = agon.col.form)

G_PC_dist.int 

ggsave(G_PC_dist.int, 
       file = "./Results/G.PC.dist.int.png", 
       width = 14, height = 10, units = "cm")

###### speculum ------
lapply(Gmat.spec, isSymmetric)  #is.symmetric.matrix
g.variances.spec = lapply(Gmat.spec, diag)
paste("Trait variances")
head(g.variances.spec)
g.variances.spec

g.eig_variances.spec = lapply(Gmat.spec, function (x) {eigen(x)$values})
paste("Eigenvalue variances")
head(g.eig_variances.spec)

g.eig_percent.spec = lapply(g.eig_variances.spec, function (x) {x/sum(x)})
g.eig_per_mat.spec = do.call(rbind, g.eig_percent.spec)
g.eig_per_mat.spec = data.frame(g.eig_per_mat.spec, rownames(g.eig_per_mat.spec))
g.eig_per.spec = melt(g.eig_per_mat.spec)
g.eig_per.spec$rownames.g.eig_per_mat.spec. <- factor(g.eig_per.spec$rownames.g.eig_per_mat.spec., 
                                                      levels = c("Upper Kai-iwi Shellbed",
                                                                 "Lower Castlecliff Shellbed",
                                                                 "Tainui Shellbed",
                                                                 "Shakespeare Cliff Basal Sand Shellbed",
                                                                 "modern"))

G_PC_dist.spec = ggplot(g.eig_per.spec,
                        aes(x = variable, y = value,
                            group = rownames.g.eig_per_mat.spec.,
                            colour = rownames.g.eig_per_mat.spec.)) +
    geom_line(aes(linetype = rownames.g.eig_per_mat.spec.)) +
    geom_point() +
    plot.theme + 
    theme_linedraw() +
    scale_y_continuous("Principal component rank",
                       limits = c(-.02, 1.02)) +
    plot.theme + 
    theme_linedraw() +
    ggtitle(expression(paste(italic("M. speculum")))) +
    scale_x_discrete("Principal component rank",
                     labels = c("PC1", "PC2")) +
    scale_color_manual(values = agon.col.form)

G_PC_dist.spec 

ggsave(G_PC_dist.spec, 
       file = "./Results/G.PC.dist.spec.png", 
       width = 14, height = 10, units = "cm")

#### NOTE: diag(Gmat.int[[3]]) has negative values!

##### G CORR -----

g.comp_mat_agon = RandomSkewers(Gmat.agon) #need at least
g.corr_mat_agon = g.comp_mat_agon$correlations + t(g.comp_mat_agon$correlations) 
diag(g.corr_mat_agon) = 1
paste("Random Skewers similarity matrix")
corrplot.mixed(g.corr_mat_agon, upper = "number", lower = "pie")

g.comp_mat_disc = RandomSkewers(Gmat.disc) #need at least
g.corr_mat_disc = g.comp_mat_disc$correlations + t(g.comp_mat_disc$correlations) 
diag(g.corr_mat_disc) = 1
paste("Random Skewers similarity matrix")
corrplot.mixed(g.corr_mat_disc, upper = "number", lower = "pie")

g.comp_mat_int = RandomSkewers(Gmat.int) #need at least
g.corr_mat_int = g.comp_mat_int$correlations + t(g.comp_mat_int$correlations) 
diag(g.corr_mat_int) = 1
paste("Random Skewers similarity matrix")
corrplot.mixed(g.corr_mat_int, upper = "number", lower = "pie")

g.comp_mat_spec = RandomSkewers(Gmat.spec) #need at least
g.corr_mat_spec = g.comp_mat_spec$correlations + t(g.comp_mat_spec$correlations) 
diag(g.corr_mat_spec) = 1
paste("Random Skewers similarity matrix")
corrplot.mixed(g.corr_mat_spec, upper = "number", lower = "pie")

#### SAVE ALL OUTPUTS ----
data.list = list(Pmat.agon, Pmat.disc,
                 Pmat.int, Pmat.spec,
                 Gmat.agon, Gmat.disc,
                 Gmat.int, Gmat.spec,
                 p.eig_variances.agon, p.eig_per_mat.disc,
                 p.eig_per_mat.int, p.eig_per_mat.spec,
                 g.eig_variances.agon, g.eig_per_mat.disc,
                 g.eig_per_mat.int, g.eig_per_mat.spec,
                 agon.mat, disc.mat,
                 int.mat, spec.mat,
                 dat_lg_N_agon, dat_lg_N_disc, 
                 dat_lg_N_int, dat_lg_N_spec,
                 form_data_agon, form_data_disc,
                 form_data_int, form_data_spec,
                 by_form_agon.n, by_form_disc.n,
                 by_form_int.n, by_form_spec.n,
                 col_form_agon.n, col_form_disc.n,
                 col_form_int.n, col_form_spec.n)

save(data.list,
     file = "./Results/data.list.RData")

load(file = "./Results/data.list.RData") #load the g matrices calculated above 

#### RAREFACTION ----
repititions <- 200

pop.size = sort(rep(c(3:500), repititions)) #make 500 so much bigger than max no. colonies

###### agonistes -----
col_form_agon.n
pop.dist.agon <- lapply(pop.size, function (x){mvrnorm(n = x, 
                                                  rep(0, 2), # num. of traits 
                                                  Gmat.agon[[1]])}) # Simulate individual trait data based on the first VCOV matrix in our time series and different sample sizes. 
# Calculate VCOV matrices 
pop.cv.agon <- lapply(pop.dist.agon, cov) 
# Estimate selection gradients based on Random Skewers, comparing our true 
# G matrix and the undersampled pooled P matrix. 
RS_result_agon = RandomSkewers(pop.cv.agon, 
                               Gmat.agon[[1]]) 
out_results_agon <- cbind(RS_result_agon$correlation, pop.size)

# Computing Random Skewers between pairs of actual G matrices in our data
#(n = 7 for 7 formations) 
# sub-setting so that we only analyze the traits and formation of interest
mean.agon.sub <- agon_mean_by_formation_colony_auto[, c(1, 4, 6)] 
#remove rows with NA so that the sample size gets correct when we plot 
#the sample size for the VCOV.
mean.agon_complete_cases <- mean.agon.sub[complete.cases(mean.agon.sub), ] #remove rows with NA so that the sample size gets correct when we plot the sample size for the VCOV.

# Calculating the sample size for each time point
colony_samples_agon = split.data.frame(mean.agon_complete_cases, mean.agon_complete_cases$time) #Sample size
sample_sizes_G_agon = lapply(colony_samples_agon, function(x){dim(x)[1]})

comp_sampleN_agon = matrix(0, 9, 9) #calculating the smallest sample size in the comparison among pairs of G 

for (i in 1:length(sample_sizes_G_agon)){
    for (j in 1:length(sample_sizes_G_agon)){
        comp_sampleN_agon[i,j] = min(as.numeric(sample_sizes_G_agon[i]), as.numeric(sample_sizes_G_agon[j]))
    }
}

# Here, we do the actual Random Skewers test and 
# combined the results (correlations in the response to selection) 
# with the smallest sample size for the pairs of G that are investigated
comp_mat_agon = RandomSkewers(Gmat.agon)
#melt_comp_agon = melt(comp_mat_agon$correlations[lower.tri(comp_mat_agon$correlations)])
values.agon <- comp_mat_agon$correlations[lower.tri(comp_mat_agon$correlations)]
melt_comp_agon <- as.data.frame(x = values.agon)
#melt_samples_agon = melt(comp_sampleN_agon[lower.tri(comp_sampleN_agon)])
values.samp.agon <- comp_sampleN_agon[lower.tri(comp_sampleN_agon)]
melt_samples_agon <- as.data.frame(x = values.samp.agon)
obs_melt_agon = cbind(melt_comp_agon, melt_samples_agon)
colnames(obs_melt_agon) = c("RS","N")

#Plotting the result
plot(out_results_agon[, 2], out_results_agon[, 1], 
     xlab = "Sample size", ylab = "Similarity", 
     pch = 19, col = "grey", cex = .5)
points(obs_melt_agon$N, obs_melt_agon$RS, 
       col = "#00BFC4", pch = 19, cex = .5)

## none are outside the gray
# previously the modern sites were, but it seems that was due to locality
# and now that has been corrected for!

###### PLOT ------
p.rare.agon <- ggplot() +
    geom_point(aes(out_results_agon[, 2], out_results_agon[, 1]),
               pch = 19, col = "grey", size = 2) +
    geom_point(aes(obs_melt_agon$N, obs_melt_agon$RS),
               col = "#00BFC4", pch = 19, size = 2) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. agonistes")))) +
    scale_x_continuous(name = "Sample size") +
    scale_y_continuous(name = "Similarity") 

ggsave(p.rare.agon, 
       file = "./Results/rarefaction.agon.png", 
       width = 14, height = 10, units = "cm")

###### discors -----
col_form_disc.n #6 coloniess
pop.dist.disc <- lapply(pop.size, function (x){mvrnorm(n = x, 
                                                       rep(0, 2), # num. of traits
                                                       Gmat.disc[[1]])}) # Simulate individual trait data based on the first VCOV matrix in our time series and different sample sizes. 
# Calculate VCOV matrices 
pop.cv.disc <- lapply(pop.dist.disc, cov) 
# Estimate selection gradients based on Random Skewers, comparing our true 
# G matrix and the undersampled pooled P matrix. 
RS_result_disc = RandomSkewers(pop.cv.disc, 
                               Gmat.disc[[1]]) 
out_results_disc <- cbind(RS_result_disc$correlation, pop.size)

# Computing Random Skewers between pairs of actual G matrices in our data
#(n = 7 for 7 formations) 
# sub-setting so that we only analyze the traits and formation of interest
mean.disc.sub <- disc_mean_by_formation_colony[, c(1, 4, 6)] 
#remove rows with NA so that the sample size gets correct when we plot 
#the sample size for the VCOV.
mean.disc_complete_cases <- mean.disc.sub[complete.cases(mean.disc.sub), ] #remove rows with NA so that the sample size gets correct when we plot the sample size for the VCOV.

# Calculating the sample size for each time point
colony_samples_disc = split.data.frame(mean.disc_complete_cases, mean.disc_complete_cases$time) #Sample size
sample_sizes_G_disc = lapply(colony_samples_disc, function(x){dim(x)[1]})

comp_sampleN_disc = matrix(0, 6, 6) #calculating the smallest sample size in the comparison among pairs of G for each formation

for (i in 1:length(sample_sizes_G_disc)){
    for (j in 1:length(sample_sizes_G_disc)){
        comp_sampleN_disc[i,j] = min(as.numeric(sample_sizes_G_disc[i]), as.numeric(sample_sizes_G_disc[j]))
    }
}

# Here, we do the actual Random Skewers test and 
# combined the results (correlations in the response to selection) 
# with the smallest sample size for the pairs of G that are investigated
comp_mat_disc = RandomSkewers(Gmat.disc)
comp_mat_disc = RandomSkewers(Gmat.disc)
#melt_comp_disc = melt(comp_mat_disc$correlations[lower.tri(comp_mat_disc$correlations)])
values.disc <- comp_mat_disc$correlations[lower.tri(comp_mat_disc$correlations)]
melt_comp_disc <- as.data.frame(x = values.disc)
#melt_samples_disc = melt(comp_sampleN_disc[lower.tri(comp_sampleN_disc)])
values.samp.disc <- comp_sampleN_disc[lower.tri(comp_sampleN_disc)]
melt_samples_disc <- as.data.frame(x = values.samp.disc)
obs_melt_disc = cbind(melt_comp_disc, melt_samples_disc)
colnames(obs_melt_disc) = c("RS","N")

#Plotting the result
plot(out_results_disc[, 2], out_results_disc[, 1], 
     xlab = "Sample size", ylab = "Similarity", 
     pch = 19, col = "grey", cex = .5)
points(obs_melt_disc$N, obs_melt_disc$RS, 
       col = "#00BFC4", pch = 19, cex = .5)

## none are outside the gray
# previously the modern sites were, but it seems that was due to locality
# and now that has been corrected for!

###### PLOT ------
p.rare.disc <- ggplot() +
    geom_point(aes(out_results_disc[, 2], out_results_disc[, 1]),
               pch = 19, col = "grey", size = 2) +
    geom_point(aes(obs_melt_disc$N, obs_melt_disc$RS),
               col = "#00BFC4", pch = 19, size = 2) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. discors")))) +
    scale_x_continuous(name = "Sample size") +
    scale_y_continuous(name = "Similarity") 

ggsave(p.rare.disc, 
       file = "./Results/rarefaction.disc.png", 
       width = 14, height = 10, units = "cm")

###### intermedia -----
col_form_int.n
pop.dist.int <- lapply(pop.size, function (x){mvrnorm(n = x, 
                                                       rep(0, 2), # num of traits 
                                                       Gmat.int[[1]])}) # Simulate individual trait data based on the first VCOV matrix in our time series and different sample sizes. 
# Calculate VCOV matrices 
pop.cv.int <- lapply(pop.dist.int, cov) 
# Estimate selection gradients based on Random Skewers, comparing our true 
# G matrix and the undersampled pooled P matrix. 
RS_result_int = RandomSkewers(pop.cv.int, 
                               Gmat.int[[1]]) 
out_results_int <- cbind(RS_result_int$correlation, pop.size)

# Computing Random Skewers between pairs of actual G matrices in our data
#(n = 7 for 7 formations) 
# sub-setting so that we only analyze the traits and formation of interest
mean.int.sub <- int_mean_by_formation_colony[, c(1, 4, 6)] 
#remove rows with NA so that the sample size gets correct when we plot 
#the sample size for the VCOV.
mean.int_complete_cases <- mean.int.sub[complete.cases(mean.int.sub), ] #remove rows with NA so that the sample size gets correct when we plot the sample size for the VCOV.

# Calculating the sample size for each time point
colony_samples_int = split.data.frame(mean.int_complete_cases, mean.int_complete_cases$time) #Sample size
sample_sizes_G_int = lapply(colony_samples_int, function(x){dim(x)[1]})

comp_sampleN_int = matrix(0, 3, 3) #calculating the smallest sample size in the comparison among pairs of G for each formation

for (i in 1:length(sample_sizes_G_int)){
    for (j in 1:length(sample_sizes_G_int)){
        comp_sampleN_int[i,j] = min(as.numeric(sample_sizes_G_int[i]), as.numeric(sample_sizes_G_int[j]))
    }
}

# Here, we do the actual Random Skewers test and 
# combined the results (correlations in the response to selection) 
# with the smallest sample size for the pairs of G that are investigated
comp_mat_int = RandomSkewers(Gmat.int)
#melt_comp_int = melt(comp_mat_int$correlations[lower.tri(comp_mat_int$correlations)])
values.int <- comp_mat_int$correlations[lower.tri(comp_mat_int$correlations)]
melt_comp_int <- as.data.frame(x = values.int)
#melt_samples_int = melt(comp_sampleN_int[lower.tri(comp_sampleN_int)])
values.samp.int <- comp_sampleN_int[lower.tri(comp_sampleN_int)]
melt_samples_int <- as.data.frame(x = values.samp.int)
obs_melt_int = cbind(melt_comp_int, melt_samples_int)
colnames(obs_melt_int) = c("RS","N")

#Plotting the result
plot(out_results_int[, 2], out_results_int[, 1], 
     xlab = "Sample size", ylab = "Similarity", 
     pch = 19, col = "grey", cex = .5)
points(obs_melt_int$N, obs_melt_int$RS, 
       col = "#00BFC4", pch = 19, cex = .5)

## none are outside the gray
# previously the modern sites were, but it seems that was due to locality
# and now that has been corrected for!

###### PLOT ------
p.rare.int <- ggplot() +
    geom_point(aes(out_results_int[, 2], out_results_int[, 1]),
               pch = 19, col = "grey", size = 2) +
    geom_point(aes(obs_melt_int$N, obs_melt_int$RS),
               col = "#00BFC4", pch = 19, size = 2) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. intermedia")))) +
    scale_x_continuous(name = "Sample size") +
    scale_y_continuous(name = "Similarity") 

ggsave(p.rare.int, 
       file = "./Results/rarefaction.int.png", 
       width = 14, height = 10, units = "cm")

###### speculum -----
col_form_spec.n
pop.dist.spec <- lapply(pop.size, function (x){mvrnorm(n = x, 
                                                       rep(0, 2), # num of traits 
                                                       Gmat.spec[[1]])}) # Simulate individual trait data based on the first VCOV matrix in our time series and different sample sizes. 
# Calculate VCOV matrices 
pop.cv.spec <- lapply(pop.dist.spec, cov) 
# Estimate selection gradients based on Random Skewers, comparing our true 
# G matrix and the undersampled pooled P matrix. 
RS_result_spec = RandomSkewers(pop.cv.spec, 
                               Gmat.spec[[1]]) 
out_results_spec <- cbind(RS_result_spec$correlation, pop.size)

# Computing Random Skewers between pairs of actual G matrices in our data
#(n = 7 for 7 formations) 
# sub-setting so that we only analyze the traits and formation of interest
mean.spec.sub <- spec_mean_by_formation_colony[, c(1, 4, 6)] 
#remove rows with NA so that the sample size gets correct when we plot 
#the sample size for the VCOV.
mean.spec_complete_cases <- mean.spec.sub[complete.cases(mean.spec.sub), ] #remove rows with NA so that the sample size gets correct when we plot the sample size for the VCOV.

# Calculating the sample size for each time point
colony_samples_spec = split.data.frame(mean.spec_complete_cases, mean.spec_complete_cases$time) #Sample size
sample_sizes_G_spec = lapply(colony_samples_spec, function(x){dim(x)[1]})

comp_sampleN_spec = matrix(0, 5, 5) #calculating the smallest sample size in the comparison among pairs of G for each formation

for (i in 1:length(sample_sizes_G_spec)){
    for (j in 1:length(sample_sizes_G_spec)){
        comp_sampleN_spec[i,j] = min(as.numeric(sample_sizes_G_spec[i]), as.numeric(sample_sizes_G_spec[j]))
    }
}

# Here, we do the actual Random Skewers test and 
# combined the results (correlations in the response to selection) 
# with the smallest sample size for the pairs of G that are investigated
comp_mat_spec = RandomSkewers(Gmat.spec)
#melt_comp_spec = melt(comp_mat_spec$correlations[lower.tri(comp_mat_spec$correlations)])
values.spec <- comp_mat_spec$correlations[lower.tri(comp_mat_spec$correlations)]
melt_comp_spec <- as.data.frame(x = values.spec)
#melt_samples_spec = melt(comp_sampleN_spec[lower.tri(comp_sampleN_spec)])
values.samp.spec <- comp_sampleN_spec[lower.tri(comp_sampleN_spec)]
melt_samples_spec <- as.data.frame(x = values.samp.spec)
obs_melt_spec = cbind(melt_comp_spec, melt_samples_spec)
colnames(obs_melt_spec) = c("RS","N")

#Plotting the result
plot(out_results_spec[, 2], out_results_spec[, 1], 
     xlab = "Sample size", ylab = "Similarity", 
     pch = 19, col = "grey", cex = .5)
points(obs_melt_spec$N, obs_melt_spec$RS, 
       col = "#00BFC4", pch = 19, cex = .5)

## none are outside the gray
# previously the modern sites were, but it seems that was due to locality
# and now that has been corrected for!

###### PLOT ------
p.rare.spec <- ggplot() +
    geom_point(aes(out_results_spec[, 2], out_results_spec[, 1]),
               pch = 19, col = "grey", size = 2) +
    geom_point(aes(obs_melt_spec$N, obs_melt_spec$RS),
               col = "#00BFC4", pch = 19, size = 2) + 
    plot.theme +
    ggtitle(expression(paste(italic("M. speculum")))) +
    scale_x_continuous(name = "Sample size") +
    scale_y_continuous(name = "Similarity") 

ggsave(p.rare.spec, 
       file = "./Results/rarefaction.spec.png", 
       width = 14, height = 10, units = "cm")

#### EVOLVABILITY ----
### Generate 10,000 selection gradients in random directions in the n-dimensional space
n_dimensions <- 2 # number of traits in G matrix
Beta <- randomBeta(10000, n_dimensions)

##### agonistes -----
#trait means by time
agon_mean_by_formation
#order of formations:
agon_form

agon_G_NKLS <- Gmat.agon$`Nukumaru Limestone`
agon_G_NKBS <- Gmat.agon$`Nukumaru Brown Sand`
agon_G_LKI <- Gmat.agon$`Lower Kai-iwi Shellbed`
agon_G_UKI <- Gmat.agon$`Upper Kai-iwi Shellbed`
agon_G_LCSB <- Gmat.agon$`Lower Castlecliff Shellbed`
agon_G_TSB <- Gmat.agon$`Tainui Shellbed`
agon_G_SHC<- Gmat.agon$`Shakespeare Cliff Basal Sand Shellbed`
agon_G_WC <- Gmat.agon$`Whanganui Core`
agon_G_mod <- Gmat.agon$modern

### Calculate the vector that defines the observed divergence between sample/formation 1
# A vector containing trait means from sample/formation
agon_NKLS_1 <- as.numeric(agon_mean_by_formation[agon_mean_by_formation == "Nukumaru Limestone", c("avg.len", "avg.wid")]) 
agon_NKBS_2 <- as.numeric(agon_mean_by_formation[agon_mean_by_formation == "Nukumaru Brown Sand", c("avg.len", "avg.wid")]) 
agon_LKI_3 <- as.numeric(agon_mean_by_formation[agon_mean_by_formation == "Lower Kai-iwi Shellbed", c("avg.len", "avg.wid")]) 
agon_UKI_4 <- as.numeric(agon_mean_by_formation[agon_mean_by_formation == "Upper Kai-iwi Shellbed", c("avg.len", "avg.wid")]) 
agon_LCSB_5 <- as.numeric(agon_mean_by_formation[agon_mean_by_formation == "Lower Castlecliff Shellbed", c("avg.len", "avg.wid")]) 
agon_TSB_6 <- as.numeric(agon_mean_by_formation[agon_mean_by_formation == "Tainui Shellbed", c("avg.len", "avg.wid")]) 
agon_SHC_7 <- as.numeric(agon_mean_by_formation[agon_mean_by_formation == "Shakespeare Cliff Basal Sand Shellbed", c("avg.len", "avg.wid")]) 
agon_WC_8 <- as.numeric(agon_mean_by_formation[agon_mean_by_formation == "Whanganui Core", c("avg.len", "avg.wid")]) 
agon_mod_9 <- as.numeric(agon_mean_by_formation[agon_mean_by_formation == "modern", c("avg.len", "avg.wid")]) 

#second - first
## really need to learn how to name things in functions...
agon_evolved_difference_unit_length_t1 <- f.normalize_vector(agon_NKBS_2 - agon_NKLS_1)
agon_evolved_difference_unit_length_t2 <- f.normalize_vector(agon_LKI_3 - agon_NKBS_2)
agon_evolved_difference_unit_length_t3 <- f.normalize_vector(agon_UKI_4 - agon_LKI_3)
agon_evolved_difference_unit_length_t4 <- f.normalize_vector(agon_LCSB_5 - agon_UKI_4)
agon_evolved_difference_unit_length_t5 <- f.normalize_vector(agon_TSB_6 - agon_LCSB_5)
agon_evolved_difference_unit_length_t6 <- f.normalize_vector(agon_SHC_7 - agon_TSB_6)
agon_evolved_difference_unit_length_t7 <- f.normalize_vector(agon_WC_8 - agon_SHC_7)
agon_evolved_difference_unit_length_t8 <- f.normalize_vector(agon_mod_9 - agon_WC_8)

###### OBSERVED EVOLVABILITY ------
### The evolvability in the direction of divergence from sample/formation 1 to sample/formation 2
#observed_evolvability_in_direction_of_change<-t(evolved_difference_unit_length)%*%as.matrix(G_matrix_1)%*%evolved_difference_unit_length
agon_observed_evolvability_in_direction_of_change_t1 <- t(agon_evolved_difference_unit_length_t1)%*%as.matrix(agon_G_NKLS)%*%agon_evolved_difference_unit_length_t1
agon_observed_evolvability_in_direction_of_change_t2 <- t(agon_evolved_difference_unit_length_t2)%*%as.matrix(agon_G_NKBS)%*%agon_evolved_difference_unit_length_t2
agon_observed_evolvability_in_direction_of_change_t3 <- t(agon_evolved_difference_unit_length_t3)%*%as.matrix(agon_G_LKI)%*%agon_evolved_difference_unit_length_t3
agon_observed_evolvability_in_direction_of_change_t4 <- t(agon_evolved_difference_unit_length_t4)%*%as.matrix(agon_G_UKI)%*%agon_evolved_difference_unit_length_t4
agon_observed_evolvability_in_direction_of_change_t5 <- t(agon_evolved_difference_unit_length_t5)%*%as.matrix(agon_G_LCSB)%*%agon_evolved_difference_unit_length_t5
agon_observed_evolvability_in_direction_of_change_t6 <- t(agon_evolved_difference_unit_length_t6)%*%as.matrix(agon_G_TSB)%*%agon_evolved_difference_unit_length_t6
agon_observed_evolvability_in_direction_of_change_t7 <- t(agon_evolved_difference_unit_length_t7)%*%as.matrix(agon_G_SHC)%*%agon_evolved_difference_unit_length_t6
agon_observed_evolvability_in_direction_of_change_t8 <- t(agon_evolved_difference_unit_length_t8)%*%as.matrix(agon_G_WC)%*%agon_evolved_difference_unit_length_t6

###### OBSERVED CONDITIONAL EVOLVABILITY ------
### The conditional evolvability in the direction of divergence
#observed_conditional_evolvability_in_direction_of_change<-1/(t(evolved_difference_unit_length)%*%solve(as.matrix(G_matrix_1))%*%evolved_difference_unit_length)
agon_observed_conditional_evolvability_in_direction_of_change_t1 <- 1/(t(agon_evolved_difference_unit_length_t1)%*%solve(as.matrix(agon_G_NKLS))%*%agon_evolved_difference_unit_length_t1)
agon_observed_conditional_evolvability_in_direction_of_change_t2 <- 1/(t(agon_evolved_difference_unit_length_t2)%*%solve(as.matrix(agon_G_NKBS))%*%agon_evolved_difference_unit_length_t2)
agon_observed_conditional_evolvability_in_direction_of_change_t3 <- 1/(t(agon_evolved_difference_unit_length_t3)%*%solve(as.matrix(agon_G_LKI))%*%agon_evolved_difference_unit_length_t3)
agon_observed_conditional_evolvability_in_direction_of_change_t4 <- 1/(t(agon_evolved_difference_unit_length_t4)%*%solve(as.matrix(agon_G_UKI))%*%agon_evolved_difference_unit_length_t4)
agon_observed_conditional_evolvability_in_direction_of_change_t5 <- 1/(t(agon_evolved_difference_unit_length_t5)%*%solve(as.matrix(agon_G_LCSB))%*%agon_evolved_difference_unit_length_t5)
agon_observed_conditional_evolvability_in_direction_of_change_t6 <- 1/(t(agon_evolved_difference_unit_length_t6)%*%solve(as.matrix(agon_G_TSB))%*%agon_evolved_difference_unit_length_t6)
agon_observed_conditional_evolvability_in_direction_of_change_t7 <- 1/(t(agon_evolved_difference_unit_length_t6)%*%solve(as.matrix(agon_G_SHC))%*%agon_evolved_difference_unit_length_t6)
agon_observed_conditional_evolvability_in_direction_of_change_t8 <- 1/(t(agon_evolved_difference_unit_length_t6)%*%solve(as.matrix(agon_G_WC))%*%agon_evolved_difference_unit_length_t6)

###### ESTIMATED CONDITIONAL EVOLVABILITY & EVOLVABILITY ------

#outputs e, r, c, a, i
#e = evolvability
#r = respondability
#c = conditional evolvability
#a = autonomy of each selection gradient
#i = integration
#Beta = matrix of selection gradients
#e and c are calculating variances of means; should not be negative
#conditional must be equal to or smaller than e; often much small

# Compute the mean, minimum and maximum evolvability (e_mean, e_min, e_max) for a G matrix based on 10,000 random selection gradients
agon_t1 <- evolvabilityBeta(as.matrix(agon_G_NKLS), Beta)
agon_sum_t1 <- summary(agon_t1) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

agon_t2 <- evolvabilityBeta(as.matrix(agon_G_NKBS), Beta)
agon_sum_t2 <- summary(agon_t2) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

agon_t3 <- evolvabilityBeta(as.matrix(agon_G_LKI), Beta)
agon_sum_t3 <- summary(agon_t3) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

agon_t4 <- evolvabilityBeta(as.matrix(agon_G_UKI), Beta)
agon_sum_t4 <- summary(agon_t4) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

agon_t5 <- evolvabilityBeta(as.matrix(agon_G_LCSB), Beta)
agon_sum_t5 <- summary(agon_t5) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

agon_t6 <- evolvabilityBeta(as.matrix(agon_G_TSB), Beta)
agon_sum_t6 <- summary(agon_t6) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

agon_t7 <- evolvabilityBeta(as.matrix(agon_G_SHC), Beta)
agon_sum_t7 <- summary(agon_t7) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

agon_t8 <- evolvabilityBeta(as.matrix(agon_G_WC), Beta)
agon_sum_t8 <- summary(agon_t8) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

agon_t9 <- evolvabilityBeta(as.matrix(agon_G_mod), Beta)
agon_sum_t9 <- summary(agon_t9) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

agon_sum <- data.frame(c.mean = c(agon_sum_t1$Averages[[3]], agon_sum_t2$Averages[[3]], agon_sum_t3$Averages[[3]],
                               agon_sum_t4$Averages[[3]], agon_sum_t5$Averages[[3]], agon_sum_t6$Averages[[3]], 
                               agon_sum_t7$Averages[[3]], agon_sum_t8$Averages[[3]], agon_sum_t9$Averages[[3]]),
                    c.min = c(agon_sum_t1$Minimum[[3]], agon_sum_t2$Minimum[[3]], agon_sum_t3$Minimum[[3]],
                              agon_sum_t4$Minimum[[3]], agon_sum_t5$Minimum[[3]], agon_sum_t6$Minimum[[3]],
                              agon_sum_t7$Minimum[[3]], agon_sum_t8$Minimum[[3]], agon_sum_t9$Minimum[[3]]),
                    c.max = c(agon_sum_t1$Maximum[[3]], agon_sum_t2$Maximum[[3]], agon_sum_t3$Maximum[[3]],
                              agon_sum_t4$Maximum[[3]], agon_sum_t5$Maximum[[3]], agon_sum_t6$Maximum[[3]],
                              agon_sum_t7$Maximum[[3]], agon_sum_t8$Maximum[[3]], agon_sum_t9$Maximum[[3]]),
                    e.mean = c(agon_sum_t1$Averages[[1]], agon_sum_t2$Averages[[1]], agon_sum_t3$Averages[[1]],
                               agon_sum_t4$Averages[[1]], agon_sum_t5$Averages[[1]], agon_sum_t6$Averages[[1]],
                               agon_sum_t7$Averages[[1]], agon_sum_t8$Averages[[1]], agon_sum_t9$Averages[[1]]),
                    e.min = c(agon_sum_t1$Minimum[[1]], agon_sum_t2$Minimum[[1]], agon_sum_t3$Minimum[[1]],
                              agon_sum_t4$Minimum[[1]], agon_sum_t5$Minimum[[1]], agon_sum_t6$Minimum[[1]],
                              agon_sum_t7$Minimum[[1]], agon_sum_t8$Minimum[[1]], agon_sum_t9$Minimum[[1]]),
                    e.max = c(agon_sum_t1$Maximum[[1]], agon_sum_t2$Maximum[[1]], agon_sum_t3$Maximum[[1]],
                              agon_sum_t4$Maximum[[1]], agon_sum_t5$Maximum[[1]], agon_sum_t6$Maximum[[1]],
                              agon_sum_t7$Minimum[[1]], agon_sum_t8$Minimum[[1]], agon_sum_t9$Minimum[[1]]),
                    observed_e = c(agon_observed_evolvability_in_direction_of_change_t1,
                                   agon_observed_evolvability_in_direction_of_change_t2,
                                   agon_observed_evolvability_in_direction_of_change_t3,
                                   agon_observed_evolvability_in_direction_of_change_t4,
                                   agon_observed_evolvability_in_direction_of_change_t5,
                                   agon_observed_evolvability_in_direction_of_change_t6,
                                   agon_observed_evolvability_in_direction_of_change_t7,
                                   agon_observed_evolvability_in_direction_of_change_t8,
                                   ""),
                    observed_c = c(agon_observed_conditional_evolvability_in_direction_of_change_t1,
                                   agon_observed_conditional_evolvability_in_direction_of_change_t2,
                                   agon_observed_conditional_evolvability_in_direction_of_change_t3,
                                   agon_observed_conditional_evolvability_in_direction_of_change_t4,
                                   agon_observed_conditional_evolvability_in_direction_of_change_t5,
                                   agon_observed_conditional_evolvability_in_direction_of_change_t6,
                                   agon_observed_conditional_evolvability_in_direction_of_change_t7,
                                   agon_observed_conditional_evolvability_in_direction_of_change_t8,
                                   ""),
                    row.names = agon_form)
#NO NEGATIVE VALUES!

write.csv(agon_sum,
          "./Results/agon.evolvability.summary.csv")

###### PLOT ------
agon_sum$formation <- rownames(agon_sum)
agon_sum$formation <- factor(agon_sum$formation,
                             levels = c("Nukumaru Limestone",
                                        "Nukumaru Brown Sand",
                                        "Lower Kai-iwi Shellbed",
                                        "Upper Kai-iwi Shellbed",
                                        "Lower Castlecliff Shellbed",
                                        "Tainui Shellbed",
                                        "Shakespeare Cliff Basal Sand Shellbed",
                                        "Whanganui Core",
                                        "modern"))

agon_sum$form.trans <- agon_form_trans
agon_sum$form.trans <- factor(agon_sum$form.trans,
                           levels = c("NKLS to NKBS", 
                                      "NKBS to LKI",
                                      "LKI to UKI",
                                      "UKI to LCSB", 
                                      "LCSB to TSB",
                                      "TSB to SHC", 
                                      "SHC to WC",
                                      "WC to modern",
                                      ""))

agon_sum.trim <- agon_sum[-9,]
p.evol.agon <- ggplot(agon_sum.trim, aes(x = form.trans)) +
    geom_boxplot(aes(ymin = e.min, 
                     lower = e.min,
                     middle = e.mean,
                     ymax = e.max,
                     upper = e.max,
                     fill = "gray"),
                 stat = "identity", fill = "gray") +
    geom_point(aes(y = as.numeric(observed_e),
                   color = "black"),
               size = 5, shape = 17, color = "black") +
    scale_x_discrete(name = "Formation",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Evolvability") +
    ggtitle(expression(paste(italic("M. agonistes")))) +
    plot.theme
#negative evolvability?

ggsave(p.evol.agon, 
       file = "./Results/agon.evolvability.png", 
       width = 14, height = 10, units = "cm")
# By comparing the evolvabilities you estimated in the direction of change (lines 9 and 12) with the average evolvabilities calculated by running line 20, you get a sense of whether evolution happened in directions with above or below average evolvability.  

##### discors -----
#trait means by time
disc_mean_by_formation
#order of formations:
disc_form

disc_G_NKLS <- Gmat.disc$`Nukumaru Limestone`
disc_G_NKBS <- Gmat.disc$`Nukumaru Brown Sand`
disc_G_LKI <- Gmat.disc$`Lower Kai-iwi Shellbed`
disc_G_UKI <- Gmat.disc$`Upper Kai-iwi Shellbed`
disc_G_TSB <- Gmat.disc$`Tainui Shellbed`
disc_G_mod <- Gmat.disc$modern

### Calculate the vector that defines the observed divergence between sample/formation 1
# A vector containing trait means from sample/formation
disc_NKLS_1 <- as.numeric(disc_mean_by_formation[disc_mean_by_formation == "Nukumaru Limestone", c("avg.len", "avg.wid")]) 
disc_NKBS_2 <- as.numeric(disc_mean_by_formation[disc_mean_by_formation == "Nukumaru Brown Sand", c("avg.len", "avg.wid")]) 
disc_LKI_3 <- as.numeric(disc_mean_by_formation[disc_mean_by_formation == "Lower Kai-iwi Shellbed", c("avg.len", "avg.wid")]) 
disc_UKI_4 <- as.numeric(disc_mean_by_formation[disc_mean_by_formation == "Upper Kai-iwi Shellbed", c("avg.len", "avg.wid")]) 
disc_TSB_5 <- as.numeric(disc_mean_by_formation[disc_mean_by_formation == "Tainui Shellbed", c("avg.len", "avg.wid")]) 
disc_mod_6 <- as.numeric(disc_mean_by_formation[disc_mean_by_formation == "modern", c("avg.len", "avg.wid")]) 

#second - first
## really need to learn how to name things in functions...
disc_evolved_difference_unit_length_t1 <- f.normalize_vector(disc_NKBS_2 - disc_NKLS_1)
disc_evolved_difference_unit_length_t2 <- f.normalize_vector(disc_LKI_3 - disc_NKBS_2)
disc_evolved_difference_unit_length_t3 <- f.normalize_vector(disc_UKI_4 - disc_LKI_3)
disc_evolved_difference_unit_length_t4 <- f.normalize_vector(disc_TSB_5 - disc_UKI_4)
disc_evolved_difference_unit_length_t5 <- f.normalize_vector(disc_mod_6 - disc_TSB_5)

###### OBSERVED EVOLVABILITY ------
### The evolvability in the direction of divergence from sample/formation 1 to sample/formation 2
#observed_evolvability_in_direction_of_change<-t(evolved_difference_unit_length)%*%as.matrix(G_matrix_1)%*%evolved_difference_unit_length
disc_observed_evolvability_in_direction_of_change_t1 <- t(disc_evolved_difference_unit_length_t1)%*%as.matrix(disc_G_NKLS)%*%disc_evolved_difference_unit_length_t1
disc_observed_evolvability_in_direction_of_change_t2 <- t(disc_evolved_difference_unit_length_t2)%*%as.matrix(disc_G_NKBS)%*%disc_evolved_difference_unit_length_t2
disc_observed_evolvability_in_direction_of_change_t3 <- t(disc_evolved_difference_unit_length_t3)%*%as.matrix(disc_G_LKI)%*%disc_evolved_difference_unit_length_t3
disc_observed_evolvability_in_direction_of_change_t4 <- t(disc_evolved_difference_unit_length_t4)%*%as.matrix(disc_G_UKI)%*%disc_evolved_difference_unit_length_t4
disc_observed_evolvability_in_direction_of_change_t5 <- t(disc_evolved_difference_unit_length_t5)%*%as.matrix(disc_G_TSB)%*%disc_evolved_difference_unit_length_t5

###### OBSERVED CONDITIONAL EVOLVABILITY ------
### The conditional evolvability in the direction of divergence
#observed_conditional_evolvability_in_direction_of_change<-1/(t(evolved_difference_unit_length)%*%solve(as.matrix(G_matrix_1))%*%evolved_difference_unit_length)
disc_observed_conditional_evolvability_in_direction_of_change_t1 <- 1/(t(disc_evolved_difference_unit_length_t1)%*%solve(as.matrix(disc_G_NKLS))%*%disc_evolved_difference_unit_length_t1)
disc_observed_conditional_evolvability_in_direction_of_change_t2 <- 1/(t(disc_evolved_difference_unit_length_t2)%*%solve(as.matrix(disc_G_NKBS))%*%disc_evolved_difference_unit_length_t2)
disc_observed_conditional_evolvability_in_direction_of_change_t3 <- 1/(t(disc_evolved_difference_unit_length_t3)%*%solve(as.matrix(disc_G_LKI))%*%disc_evolved_difference_unit_length_t3)
disc_observed_conditional_evolvability_in_direction_of_change_t4 <- 1/(t(disc_evolved_difference_unit_length_t4)%*%solve(as.matrix(disc_G_UKI))%*%disc_evolved_difference_unit_length_t4)
disc_observed_conditional_evolvability_in_direction_of_change_t5 <- 1/(t(disc_evolved_difference_unit_length_t5)%*%solve(as.matrix(disc_G_TSB))%*%disc_evolved_difference_unit_length_t5)

###### ESTIMATED CONDITIONAL EVOLVABILITY & EVOLVABILITY ------

#outputs e, r, c, a, i
#e = evolvability
#r = respondability
#c = conditional evolvability
#a = autonomy of each selection gradient
#i = integration
#Beta = matrix of selection gradients
#e and c are calculating variances of means; should not be negative
#conditional must be equal to or smaller than e; often much small

# Compute the mean, minimum and maximum evolvability (e_mean, e_min, e_max) for a G matrix based on 10,000 random selection gradients
disc_t1 <- evolvabilityBeta(as.matrix(disc_G_NKLS), Beta)
disc_sum_t1 <- summary(disc_t1) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

disc_t2 <- evolvabilityBeta(as.matrix(disc_G_NKBS), Beta)
disc_sum_t2 <- summary(disc_t2) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

disc_t3 <- evolvabilityBeta(as.matrix(disc_G_LKI), Beta)
disc_sum_t3 <- summary(disc_t3) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

disc_t4 <- evolvabilityBeta(as.matrix(disc_G_UKI), Beta)
disc_sum_t4 <- summary(disc_t4) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

disc_t5 <- evolvabilityBeta(as.matrix(disc_G_TSB), Beta)
disc_sum_t5 <- summary(disc_t5) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

disc_t6 <- evolvabilityBeta(as.matrix(disc_G_mod), Beta)
disc_sum_t6 <- summary(disc_t6) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

disc_sum <- data.frame(c.mean = c(disc_sum_t1$Averages[[3]], disc_sum_t2$Averages[[3]], disc_sum_t3$Averages[[3]],
                                  disc_sum_t4$Averages[[3]], disc_sum_t5$Averages[[3]], disc_sum_t6$Averages[[3]]),
                       c.min = c(disc_sum_t1$Minimum[[3]], disc_sum_t2$Minimum[[3]], disc_sum_t3$Minimum[[3]],
                                 disc_sum_t4$Minimum[[3]], disc_sum_t5$Minimum[[3]], disc_sum_t6$Minimum[[3]]),
                       c.max = c(disc_sum_t1$Maximum[[3]], disc_sum_t2$Maximum[[3]], disc_sum_t3$Maximum[[3]],
                                 disc_sum_t4$Maximum[[3]], disc_sum_t5$Maximum[[3]], disc_sum_t6$Maximum[[3]]),
                       e.mean = c(disc_sum_t1$Averages[[1]], disc_sum_t2$Averages[[1]], disc_sum_t3$Averages[[1]],
                                  disc_sum_t4$Averages[[1]], disc_sum_t5$Averages[[1]], disc_sum_t6$Averages[[1]]),
                       e.min = c(disc_sum_t1$Minimum[[1]], disc_sum_t2$Minimum[[1]], disc_sum_t3$Minimum[[1]],
                                 disc_sum_t4$Minimum[[1]], disc_sum_t5$Minimum[[1]], disc_sum_t6$Minimum[[1]]),
                       e.max = c(disc_sum_t1$Maximum[[1]], disc_sum_t2$Maximum[[1]], disc_sum_t3$Maximum[[1]],
                                 disc_sum_t4$Maximum[[1]], disc_sum_t5$Maximum[[1]], disc_sum_t6$Maximum[[1]]),
                       observed_e = c(disc_observed_evolvability_in_direction_of_change_t1,
                                      disc_observed_evolvability_in_direction_of_change_t2,
                                      disc_observed_evolvability_in_direction_of_change_t3,
                                      disc_observed_evolvability_in_direction_of_change_t4,
                                      disc_observed_evolvability_in_direction_of_change_t5,
                                      ""),
                       observed_c = c(disc_observed_conditional_evolvability_in_direction_of_change_t1,
                                      disc_observed_conditional_evolvability_in_direction_of_change_t2,
                                      disc_observed_conditional_evolvability_in_direction_of_change_t3,
                                      disc_observed_conditional_evolvability_in_direction_of_change_t4,
                                      disc_observed_conditional_evolvability_in_direction_of_change_t5,
                                      ""),
                       row.names = disc_form)
#NO NEGATIVE VALUES!

write.csv(disc_sum,
          "./Results/disc.evolvability.summary.csv")

###### PLOT ------
disc_sum$formation <- rownames(disc_sum)
disc_sum$formation <- factor(disc_sum$formation,
                             levels = c("Nukumaru Limestone",
                                        "Nukumaru Brown Sand",
                                        "Lower Kai-iwi Shellbed",
                                        "Upper Kai-iwi Shellbed",
                                        "Tainui Shellbed",
                                        "modern"))

disc_sum$form.trans <- disc_form_trans
disc_sum$form.trans <- factor(disc_sum$form.trans,
                              levels = c("NKLS to NKBS", 
                                         "NKBS to LKI",
                                         "LKI to UKI",
                                         "UKI to TSB",
                                         "TSB to modern",
                                         ""))

disc_sum.trim <- disc_sum[-6,]
p.evol.disc <- ggplot(disc_sum.trim, aes(x = form.trans)) +
    geom_boxplot(aes(ymin = e.min, 
                     lower = e.min,
                     middle = e.mean,
                     ymax = e.max,
                     upper = e.max,
                     fill = "gray"),
                 stat = "identity", fill = "gray") +
    geom_point(aes(y = as.numeric(observed_e),
                   color = "black"),
               size = 5, shape = 17, color = "black") +
    scale_x_discrete(name = "Formation",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Evolvability") +
    ggtitle(expression(paste(italic("M. discors")))) +
    plot.theme
#negative evolvability?

ggsave(p.evol.disc, 
       file = "./Results/disc.evolvability.png", 
       width = 14, height = 10, units = "cm")
# By comparing the evolvabilities you estimated in the direction of change (lines 9 and 12) with the average evolvabilities calculated by running line 20, you get a sense of whether evolution happened in directions with above or below average evolvability.  

##### intermedia -----
#trait means by time
int_mean_by_formation
#order of formations:
int_form

int_G_LCSB <- Gmat.int$`Lower Castlecliff Shellbed`
int_G_WC <- Gmat.int$`Whanganui Core`
int_G_mod <- Gmat.int$modern

### Calculate the vector that defines the observed divergence between sample/formation 1
# A vector containing trait means from sample/formation
int_LCSB_1 <- as.numeric(int_mean_by_formation[int_mean_by_formation == "Lower Castlecliff Shellbed", c("avg.len", "avg.wid")]) 
int_WC_2 <- as.numeric(int_mean_by_formation[int_mean_by_formation == "Whanganui Core", c("avg.len", "avg.wid")]) 
int_mod_3 <- as.numeric(int_mean_by_formation[int_mean_by_formation == "modern", c("avg.len", "avg.wid")]) 

#second - first
## really need to learn how to name things in functions...
int_evolved_difference_unit_length_t1 <- f.normalize_vector(int_WC_2 - int_LCSB_1)
int_evolved_difference_unit_length_t2 <- f.normalize_vector(int_mod_3 - int_WC_2)

###### OBSERVED EVOLVABILITY ------
### The evolvability in the direction of divergence from sample/formation 1 to sample/formation 2
#observed_evolvability_in_direction_of_change<-t(evolved_difference_unit_length)%*%as.matrix(G_matrix_1)%*%evolved_difference_unit_length
int_observed_evolvability_in_direction_of_change_t1 <- t(int_evolved_difference_unit_length_t1)%*%as.matrix(int_G_LCSB)%*%int_evolved_difference_unit_length_t1
int_observed_evolvability_in_direction_of_change_t2 <- t(int_evolved_difference_unit_length_t2)%*%as.matrix(int_G_WC)%*%int_evolved_difference_unit_length_t2

###### OBSERVED CONDITIONAL EVOLVABILITY ------
### The conditional evolvability in the direction of divergence
#observed_conditional_evolvability_in_direction_of_change<-1/(t(evolved_difference_unit_length)%*%solve(as.matrix(G_matrix_1))%*%evolved_difference_unit_length)
int_observed_conditional_evolvability_in_direction_of_change_t1 <- 1/(t(int_evolved_difference_unit_length_t1)%*%solve(as.matrix(int_G_LCSB))%*%int_evolved_difference_unit_length_t1)
int_observed_conditional_evolvability_in_direction_of_change_t2 <- 1/(t(int_evolved_difference_unit_length_t2)%*%solve(as.matrix(int_G_WC))%*%int_evolved_difference_unit_length_t2)

###### ESTIMATED CONDITIONAL EVOLVABILITY & EVOLVABILITY ------

#outputs e, r, c, a, i
#e = evolvability
#r = respondability
#c = conditional evolvability
#a = autonomy of each selection gradient
#i = integration
#Beta = matrix of selection gradients
#e and c are calculating variances of means; should not be negative
#conditional must be equal to or smaller than e; often much small

# Compute the mean, minimum and maximum evolvability (e_mean, e_min, e_max) for a G matrix based on 10,000 random selection gradients
int_t1 <- evolvabilityBeta(as.matrix(int_G_LCSB), Beta)
int_sum_t1 <- summary(int_t1) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

int_t2 <- evolvabilityBeta(as.matrix(int_G_WC), Beta)
int_sum_t2 <- summary(int_t2) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

int_t3 <- evolvabilityBeta(as.matrix(int_G_mod), Beta)
int_sum_t3 <- summary(int_t3) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

int_sum <- data.frame(c.mean = c(int_sum_t1$Averages[[3]], int_sum_t2$Averages[[3]], int_sum_t3$Averages[[3]]),
                       c.min = c(int_sum_t1$Minimum[[3]], int_sum_t2$Minimum[[3]], int_sum_t3$Minimum[[3]]),
                       c.max = c(int_sum_t1$Maximum[[3]], int_sum_t2$Maximum[[3]], int_sum_t3$Maximum[[3]]),
                       e.mean = c(int_sum_t1$Averages[[1]], int_sum_t2$Averages[[1]], int_sum_t3$Averages[[1]]),
                       e.min = c(int_sum_t1$Minimum[[1]], int_sum_t2$Minimum[[1]], int_sum_t3$Minimum[[1]]),
                       e.max = c(int_sum_t1$Maximum[[1]], int_sum_t2$Maximum[[1]], int_sum_t3$Maximum[[1]]),
                       observed_e = c(int_observed_evolvability_in_direction_of_change_t1,
                                      int_observed_evolvability_in_direction_of_change_t2,
                                      ""),
                       observed_c = c(int_observed_conditional_evolvability_in_direction_of_change_t1,
                                      int_observed_conditional_evolvability_in_direction_of_change_t2,
                                      ""),
                       row.names = int_form)
#NO NEGATIVE VALUES!

write.csv(int_sum,
          "./Results/int.evolvability.summary.csv")

###### PLOT ------
int_sum$formation <- rownames(int_sum)
int_sum$formation <- factor(int_sum$formation,
                             levels = c("Lower Castlecliff Shellbed",
                                        "Whanganui Core",
                                        "modern"))

int_sum$form.trans <- int_form_trans
int_sum$form.trans <- factor(int_sum$form.trans,
                              levels = c("LCSB to WC", 
                                         "WC to modern",
                                         ""))

int_sum.trim <- int_sum[-3,]
p.evol.int <- ggplot(int_sum.trim, aes(x = form.trans)) +
    geom_boxplot(aes(ymin = e.min, 
                     lower = e.min,
                     middle = e.mean,
                     ymax = e.max,
                     upper = e.max,
                     fill = "gray"),
                 stat = "identity", fill = "gray") +
    geom_point(aes(y = as.numeric(observed_e),
                   color = "black"),
               size = 5, shape = 17, color = "black") +
    scale_x_discrete(name = "Formation",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Evolvability") +
    ggtitle(expression(paste(italic("M. intermedia")))) +
    plot.theme
#negative evolvability?

ggsave(p.evol.int, 
       file = "./Results/int.evolvability.png", 
       width = 14, height = 10, units = "cm")
# By comparing the evolvabilities you estimated in the direction of change (lines 9 and 12) with the average evolvabilities calculated by running line 20, you get a sense of whether evolution happened in directions with above or below average evolvability.  

##### speculum -----
#trait means by time
spec_mean_by_formation
#order of formations:
spec_form

spec_G_UKI <- Gmat.spec$`Upper Kai-iwi Shellbed`
spec_G_LCSB <- Gmat.spec$`Lower Castlecliff Shellbed`
spec_G_TSB <- Gmat.spec$`Tainui Shellbed`
spec_G_SHC<- Gmat.spec$`Shakespeare Cliff Basal Sand Shellbed`
spec_G_mod <- Gmat.spec$modern

### Calculate the vector that defines the observed divergence between sample/formation 1
# A vector containing trait means from sample/formation
spec_UKI_1 <- as.numeric(spec_mean_by_formation[spec_mean_by_formation == "Upper Kai-iwi Shellbed", c("avg.len", "avg.wid")]) 
spec_LCSB_2 <- as.numeric(spec_mean_by_formation[spec_mean_by_formation == "Lower Castlecliff Shellbed", c("avg.len", "avg.wid")]) 
spec_TSB_3 <- as.numeric(spec_mean_by_formation[spec_mean_by_formation == "Tainui Shellbed", c("avg.len", "avg.wid")]) 
spec_SHC_4 <- as.numeric(spec_mean_by_formation[spec_mean_by_formation == "Shakespeare Cliff Basal Sand Shellbed", c("avg.len", "avg.wid")]) 
spec_mod_5 <- as.numeric(spec_mean_by_formation[spec_mean_by_formation == "modern", c("avg.len", "avg.wid")]) 

#second - first
## really need to learn how to name things in functions...
spec_evolved_difference_unit_length_t1 <- f.normalize_vector(spec_LCSB_2 - spec_UKI_1)
spec_evolved_difference_unit_length_t2 <- f.normalize_vector(spec_TSB_3 - spec_LCSB_2)
spec_evolved_difference_unit_length_t3 <- f.normalize_vector(spec_SHC_4 - spec_TSB_3)
spec_evolved_difference_unit_length_t4 <- f.normalize_vector(spec_mod_5 - spec_SHC_4)

###### OBSERVED EVOLVABILITY ------
### The evolvability in the direction of divergence from sample/formation 1 to sample/formation 2
#observed_evolvability_in_direction_of_change<-t(evolved_difference_unit_length)%*%as.matrix(G_matrix_1)%*%evolved_difference_unit_length
spec_observed_evolvability_in_direction_of_change_t1 <- t(spec_evolved_difference_unit_length_t1)%*%as.matrix(spec_G_UKI)%*%spec_evolved_difference_unit_length_t1
spec_observed_evolvability_in_direction_of_change_t2 <- t(spec_evolved_difference_unit_length_t2)%*%as.matrix(spec_G_LCSB)%*%spec_evolved_difference_unit_length_t2
spec_observed_evolvability_in_direction_of_change_t3 <- t(spec_evolved_difference_unit_length_t3)%*%as.matrix(spec_G_TSB)%*%spec_evolved_difference_unit_length_t3
spec_observed_evolvability_in_direction_of_change_t4 <- t(spec_evolved_difference_unit_length_t4)%*%as.matrix(spec_G_SHC)%*%spec_evolved_difference_unit_length_t4

###### OBSERVED CONDITIONAL EVOLVABILITY ------
### The conditional evolvability in the direction of divergence
#observed_conditional_evolvability_in_direction_of_change<-1/(t(evolved_difference_unit_length)%*%solve(as.matrix(G_matrix_1))%*%evolved_difference_unit_length)
spec_observed_conditional_evolvability_in_direction_of_change_t1 <- 1/(t(spec_evolved_difference_unit_length_t1)%*%solve(as.matrix(spec_G_UKI))%*%spec_evolved_difference_unit_length_t1)
spec_observed_conditional_evolvability_in_direction_of_change_t2 <- 1/(t(spec_evolved_difference_unit_length_t2)%*%solve(as.matrix(spec_G_LCSB))%*%spec_evolved_difference_unit_length_t2)
spec_observed_conditional_evolvability_in_direction_of_change_t3 <- 1/(t(spec_evolved_difference_unit_length_t3)%*%solve(as.matrix(spec_G_TSB))%*%spec_evolved_difference_unit_length_t3)
spec_observed_conditional_evolvability_in_direction_of_change_t4 <- 1/(t(spec_evolved_difference_unit_length_t4)%*%solve(as.matrix(spec_G_SHC))%*%spec_evolved_difference_unit_length_t4)

###### ESTIMATED CONDITIONAL EVOLVABILITY & EVOLVABILITY ------

#outputs e, r, c, a, i
#e = evolvability
#r = respondability
#c = conditional evolvability
#a = autonomy of each selection gradient
#i = integration
#Beta = matrix of selection gradients
#e and c are calculating variances of means; should not be negative
#conditional must be equal to or smaller than e; often much small

# Compute the mean, minimum and maximum evolvability (e_mean, e_min, e_max) for a G matrix based on 10,000 random selection gradients
spec_t1 <- evolvabilityBeta(as.matrix(spec_G_UKI), Beta)
spec_sum_t1 <- summary(spec_t1) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

spec_t2 <- evolvabilityBeta(as.matrix(spec_G_LCSB), Beta)
spec_sum_t2 <- summary(spec_t2) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

spec_t3 <- evolvabilityBeta(as.matrix(spec_G_TSB), Beta)
spec_sum_t3 <- summary(spec_t3) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

spec_t4 <- evolvabilityBeta(as.matrix(spec_G_SHC), Beta)
spec_sum_t4 <- summary(spec_t4) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

spec_t5 <- evolvabilityBeta(as.matrix(spec_G_mod), Beta)
spec_sum_t5 <- summary(spec_t5) #provides you with info on mean, minimum and maximum evolvability  (e_mean, e_min, e_max) and conditional evolvability  (c_mean, c_min, c_max) for a given G matrix

spec_sum <- data.frame(c.mean = c(spec_sum_t1$Averages[[3]], spec_sum_t2$Averages[[3]], spec_sum_t3$Averages[[3]],
                                  spec_sum_t4$Averages[[3]], spec_sum_t5$Averages[[3]]),
                       c.min = c(spec_sum_t1$Minimum[[3]], spec_sum_t2$Minimum[[3]], spec_sum_t3$Minimum[[3]],
                                 spec_sum_t4$Minimum[[3]], spec_sum_t5$Minimum[[3]]),
                       c.max = c(spec_sum_t1$Maximum[[3]], spec_sum_t2$Maximum[[3]], spec_sum_t3$Maximum[[3]],
                                 spec_sum_t4$Maximum[[3]], spec_sum_t5$Maximum[[3]]),
                       e.mean = c(spec_sum_t1$Averages[[1]], spec_sum_t2$Averages[[1]], spec_sum_t3$Averages[[1]],
                                  spec_sum_t4$Averages[[1]], spec_sum_t5$Averages[[1]]),
                       e.min = c(spec_sum_t1$Minimum[[1]], spec_sum_t2$Minimum[[1]], spec_sum_t3$Minimum[[1]],
                                 spec_sum_t4$Minimum[[1]], spec_sum_t5$Minimum[[1]]),
                       e.max = c(spec_sum_t1$Maximum[[1]], spec_sum_t2$Maximum[[1]], spec_sum_t3$Maximum[[1]],
                                 spec_sum_t4$Maximum[[1]], spec_sum_t5$Maximum[[1]]),
                       observed_e = c(spec_observed_evolvability_in_direction_of_change_t1,
                                      spec_observed_evolvability_in_direction_of_change_t2,
                                      spec_observed_evolvability_in_direction_of_change_t3,
                                      spec_observed_evolvability_in_direction_of_change_t4,
                                      ""),
                       observed_c = c(spec_observed_conditional_evolvability_in_direction_of_change_t1,
                                      spec_observed_conditional_evolvability_in_direction_of_change_t2,
                                      spec_observed_conditional_evolvability_in_direction_of_change_t3,
                                      spec_observed_conditional_evolvability_in_direction_of_change_t4,
                                      ""),
                       row.names = spec_form)
#NO NEGATIVE VALUES!

write.csv(spec_sum,
          "./Results/spec.evolvability.summary.csv")

###### PLOT ------
spec_sum$formation <- rownames(spec_sum)
spec_sum$formation <- factor(spec_sum$formation,
                             levels = c("Upper Kai-iwi Shellbed",
                                        "Lower Castlecliff Shellbed",
                                        "Tainui Shellbed",
                                        "Shakespeare Cliff Basal Sand Shellbed",
                                        "modern"))

spec_sum$form.trans <- spec_form_trans
spec_sum$form.trans <- factor(spec_sum$form.trans,
                              levels = c("UKI to LCSB", 
                                         "LCSB to TSB",
                                         "TSB to SHC", 
                                         "SHC to modern",
                                         ""))

spec_sum.trim <- spec_sum[-5,]
p.evol.spec <- ggplot(spec_sum.trim, aes(x = form.trans)) +
    geom_boxplot(aes(ymin = e.min, 
                     lower = e.min,
                     middle = e.mean,
                     ymax = e.max,
                     upper = e.max,
                     fill = "gray"),
                 stat = "identity", fill = "gray") +
    geom_point(aes(y = as.numeric(observed_e),
                   color = "black"),
               size = 5, shape = 17, color = "black") +
    scale_x_discrete(name = "Formation",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Evolvability") +
    ggtitle(expression(paste(italic("M. speculum")))) +
    plot.theme
#negative evolvability?

ggsave(p.evol.spec, 
       file = "./Results/spec.evolvability.png", 
       width = 14, height = 10, units = "cm")
# By comparing the evolvabilities you estimated in the direction of change (lines 9 and 12) with the average evolvabilities calculated by running line 20, you get a sense of whether evolution happened in directions with above or below average evolvability.  

#### G OVER TIME ----
#change in G max between formations

##### AGONISTES -----
### Proportion of variance in n-dimensional trait space that is explained by PC1 (i.e., the first eigenvector)
#eigen(as.matrix(G_matrix_1))$values[1]/sum(eigen(as.matrix(G_matrix_1))$values)
eigen(as.matrix(agon_G_NKLS))$values[1]/sum(eigen(as.matrix(agon_G_NKLS))$values) 
eigen(as.matrix(agon_G_NKBS))$values[1]/sum(eigen(as.matrix(agon_G_NKBS))$values) 
eigen(as.matrix(agon_G_LKI))$values[1]/sum(eigen(as.matrix(agon_G_LKI))$values) 
eigen(as.matrix(agon_G_UKI))$values[1]/sum(eigen(as.matrix(agon_G_UKI))$values) 
eigen(as.matrix(agon_G_LCSB))$values[1]/sum(eigen(as.matrix(agon_G_LCSB))$values) 
eigen(as.matrix(agon_G_TSB))$values[1]/sum(eigen(as.matrix(agon_G_TSB))$values) 
eigen(as.matrix(agon_G_SHC))$values[1]/sum(eigen(as.matrix(agon_G_SHC))$values) 
eigen(as.matrix(agon_G_WC))$values[1]/sum(eigen(as.matrix(agon_G_WC))$values) 
eigen(as.matrix(agon_G_mod))$values[1]/sum(eigen(as.matrix(agon_G_mod))$values) 
#close to 100% for all

### How much is the direction of Gmax (i.e., the direction first ) varying between different G-matrices? 
agon_Gmax_NKLS <- eigen(agon_G_NKLS)$vectors[,1]
agon_Gmax_NKBS <- eigen(agon_G_NKBS)$vectors[,1]
agon_Gmax_LKI <- eigen(agon_G_LKI)$vectors[,1]
agon_Gmax_UKI <- eigen(agon_G_UKI)$vectors[,1]
agon_Gmax_LCSB <- eigen(agon_G_LCSB)$vectors[,1]
agon_Gmax_TSB <- eigen(agon_G_TSB)$vectors[,1]
agon_Gmax_SHC <- eigen(agon_G_SHC)$vectors[,1]
agon_Gmax_WC <- eigen(agon_G_WC)$vectors[,1]
agon_Gmax_mod <- eigen(agon_G_mod)$vectors[,1]

# Put Gmax to norm length
agon_Gmax_NKLS_norm <- f.normalize_vector(agon_Gmax_NKLS)
agon_Gmax_NKBS_norm <- f.normalize_vector(agon_Gmax_NKBS)
agon_Gmax_LKI_norm <- f.normalize_vector(agon_Gmax_LKI)
agon_Gmax_UKI_norm <- f.normalize_vector(agon_Gmax_UKI)
agon_Gmax_LCSB_norm <- f.normalize_vector(agon_Gmax_LCSB)
agon_Gmax_TSB_norm <- f.normalize_vector(agon_Gmax_TSB)
agon_Gmax_SHC_norm <- f.normalize_vector(agon_Gmax_SHC)
agon_Gmax_WC_norm <- f.normalize_vector(agon_Gmax_WC)
agon_Gmax_mod_norm <- f.normalize_vector(agon_Gmax_mod)

# Calculate the dot product of the unit vectors; tells number 0 to 1
agon_dot_product.Gmax_NKLS_NKBS <- sum(agon_Gmax_NKLS_norm * agon_Gmax_NKBS_norm) 
# Calculate the angle in degrees
agon_angle_radians.Gmax_NKLS_NKBS <- acos(agon_dot_product.Gmax_NKLS_NKBS) * (180 / pi)

agon_dot_product.Gmax_NKBS_LKI <- sum(agon_Gmax_NKBS_norm * agon_Gmax_LKI_norm) 
# Calculate the angle in degrees
agon_angle_radians.Gmax_NKBS_LKI <- acos(agon_dot_product.Gmax_NKBS_LKI) * (180 / pi)

agon_dot_product.Gmax_LKI_UKI <- sum(agon_Gmax_LKI_norm * agon_Gmax_UKI_norm) 
# Calculate the angle in degrees
agon_angle_radians.Gmax_LKI_UKI <- acos(agon_dot_product.Gmax_LKI_UKI) * (180 / pi)

agon_dot_product.Gmax_UKI_LCSB <- sum(agon_Gmax_UKI_norm * agon_Gmax_LCSB_norm) 
# Calculate the angle in degrees
agon_angle_radians.Gmax_UKI_LCSB <- acos(agon_dot_product.Gmax_UKI_LCSB) * (180 / pi)

agon_dot_product.Gmax_LCSB_TSB <- sum(agon_Gmax_LCSB_norm * agon_Gmax_TSB_norm) 
# Calculate the angle in degrees
agon_angle_radians.Gmax_LCSB_TSB <- acos(agon_dot_product.Gmax_LCSB_TSB) * (180 / pi)

agon_dot_product.Gmax_TSB_SHC <- sum(agon_Gmax_TSB_norm * agon_Gmax_SHC_norm) 
# Calculate the angle in degrees
agon_angle_radians.Gmax_TSB_SHC <- acos(agon_dot_product.Gmax_TSB_SHC) * (180 / pi)

agon_dot_product.Gmax_SHC_WC <- sum(agon_Gmax_SHC_norm * agon_Gmax_WC_norm) 
# Calculate the angle in degrees
agon_angle_radians.Gmax_SHC_WC <- acos(agon_dot_product.Gmax_SHC_WC) * (180 / pi)

agon_dot_product.Gmax_WC_mod <- sum(agon_Gmax_WC_norm * agon_Gmax_mod_norm) 
# Calculate the angle in degrees
agon_angle_radians.Gmax_WC_mod <- acos(agon_dot_product.Gmax_WC_mod) * (180 / pi)


agon_corr.diff_Gs <- c(agon_dot_product.Gmax_NKLS_NKBS,
                       agon_dot_product.Gmax_NKBS_LKI,
                       agon_dot_product.Gmax_LKI_UKI, 
                       agon_dot_product.Gmax_UKI_LCSB,
                       agon_dot_product.Gmax_LCSB_TSB, 
                       agon_dot_product.Gmax_TSB_SHC,
                       agon_dot_product.Gmax_SHC_WC,
                       agon_dot_product.Gmax_WC_mod)

agon_angle_diff_Gs <- c(agon_angle_radians.Gmax_NKLS_NKBS, 
                        agon_angle_radians.Gmax_NKBS_LKI,
                        agon_angle_radians.Gmax_LKI_UKI, 
                        agon_angle_radians.Gmax_UKI_LCSB, 
                        agon_angle_radians.Gmax_LCSB_TSB, 
                        agon_angle_radians.Gmax_TSB_SHC,
                        agon_angle_radians.Gmax_SHC_WC,
                        agon_angle_radians.Gmax_WC_mod)

agon_diff_between_Gs <- as.data.frame(cbind(agon_angle_diff_Gs, agon_corr.diff_Gs))
agon_diff_between_Gs$agon_angle_diff_Gs <- as.numeric(agon_diff_between_Gs$agon_angle_diff_Gs)
agon_diff_between_Gs$agon_angle.diff_Gs.time <- agon_form_trans[-9]
agon_diff_between_Gs$agon_angle.diff_Gs.time <- factor(agon_diff_between_Gs$agon_angle.diff_Gs.time,
                                                       levels = c("NKLS to NKBS", 
                                                                  "NKBS to LKI",
                                                                  "LKI to UKI",
                                                                  "UKI to LCSB", 
                                                                  "LCSB to TSB",
                                                                  "TSB to SHC", 
                                                                  "SHC to WC",
                                                                  "WC to modern"))

for(i in 1:nrow(agon_diff_between_Gs)){
    if(isTRUE(agon_diff_between_Gs$agon_angle_diff_Gs[i] > 90)){
        agon_diff_between_Gs$agon_angle_diff_Gs[i] <- 180 - agon_diff_between_Gs$agon_angle_diff_Gs[i]
    }
    else{
        next
    }
}

write.csv(agon_diff_between_Gs,
          "./Results/agon.differences.between.Gs.csv",
          row.names = FALSE)

###### PLOT ------
p.agon_ang_g <- ggplot(agon_diff_between_Gs) +
    geom_point(aes(x = agon_angle.diff_Gs.time, y = agon_angle_diff_Gs),
               size = 7, shape = 19) +
    scale_x_discrete(name = "Formation Transition",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Angle difference between G matrices", 
                       lim = c(0, 90)) + 
    geom_hline(yintercept = 45,
               lty = 2, col = "lightgray",
               size = 1) +
    ggtitle(expression(paste(italic("M. agonistes")))) +
    plot.theme

ggsave(p.agon_ang_g, 
       file = "./Results/agon.angle.g.diff.png", 
       width = 20, height = 20, units = "cm")

##### DISCORS -----
### Proportion of variance in n-dimensional trait space that is explained by PC1 (i.e., the first eigenvector)
#eigen(as.matrix(G_matrix_1))$values[1]/sum(eigen(as.matrix(G_matrix_1))$values)
eigen(as.matrix(disc_G_NKLS))$values[1]/sum(eigen(as.matrix(disc_G_NKLS))$values) 
eigen(as.matrix(disc_G_NKBS))$values[1]/sum(eigen(as.matrix(disc_G_NKBS))$values) 
eigen(as.matrix(disc_G_LKI))$values[1]/sum(eigen(as.matrix(disc_G_LKI))$values) 
eigen(as.matrix(disc_G_UKI))$values[1]/sum(eigen(as.matrix(disc_G_UKI))$values)
eigen(as.matrix(disc_G_TSB))$values[1]/sum(eigen(as.matrix(disc_G_TSB))$values) 
eigen(as.matrix(disc_G_mod))$values[1]/sum(eigen(as.matrix(disc_G_mod))$values) 
#half to over 80%

### How much is the direction of Gmax (i.e., the direction first ) varying between different G-matrices? 
disc_Gmax_NKLS <- eigen(disc_G_NKLS)$vectors[,1]
disc_Gmax_NKBS <- eigen(disc_G_NKBS)$vectors[,1]
disc_Gmax_LKI <- eigen(disc_G_LKI)$vectors[,1]
disc_Gmax_UKI <- eigen(disc_G_UKI)$vectors[,1]
disc_Gmax_TSB <- eigen(disc_G_TSB)$vectors[,1]
disc_Gmax_mod <- eigen(disc_G_mod)$vectors[,1]

# Put Gmax to norm length
disc_Gmax_NKLS_norm <- f.normalize_vector(disc_Gmax_NKLS)
disc_Gmax_NKBS_norm <- f.normalize_vector(disc_Gmax_NKBS)
disc_Gmax_LKI_norm <- f.normalize_vector(disc_Gmax_LKI)
disc_Gmax_UKI_norm <- f.normalize_vector(disc_Gmax_UKI)
disc_Gmax_TSB_norm <- f.normalize_vector(disc_Gmax_TSB)
disc_Gmax_mod_norm <- f.normalize_vector(disc_Gmax_mod)

# Calculate the dot product of the unit vectors; tells number 0 to 1
disc_dot_product.Gmax_NKLS_NKBS <- sum(disc_Gmax_NKLS_norm * disc_Gmax_NKBS_norm) 
# Calculate the angle in degrees
disc_angle_radians.Gmax_NKLS_NKBS <- acos(disc_dot_product.Gmax_NKLS_NKBS) * (180 / pi)

disc_dot_product.Gmax_NKBS_LKI <- sum(disc_Gmax_NKBS_norm * disc_Gmax_LKI_norm) 
# Calculate the angle in degrees
disc_angle_radians.Gmax_NKBS_LKI <- acos(disc_dot_product.Gmax_NKBS_LKI) * (180 / pi)

disc_dot_product.Gmax_LKI_UKI <- sum(disc_Gmax_LKI_norm * disc_Gmax_UKI_norm) 
# Calculate the angle in degrees
disc_angle_radians.Gmax_LKI_UKI <- acos(disc_dot_product.Gmax_LKI_UKI) * (180 / pi)

disc_dot_product.Gmax_UKI_TSB <- sum(disc_Gmax_UKI_norm * disc_Gmax_TSB_norm) 
# Calculate the angle in degrees
disc_angle_radians.Gmax_UKI_TSB <- acos(disc_dot_product.Gmax_UKI_TSB) * (180 / pi)

disc_dot_product.Gmax_TSB_mod <- sum(disc_Gmax_TSB_norm * disc_Gmax_mod_norm) 
# Calculate the angle in degrees
disc_angle_radians.Gmax_TSB_mod <- acos(disc_dot_product.Gmax_TSB_mod) * (180 / pi)

disc_corr.diff_Gs <- c(disc_dot_product.Gmax_NKLS_NKBS,
                       disc_dot_product.Gmax_NKBS_LKI,
                       disc_dot_product.Gmax_LKI_UKI, 
                       disc_dot_product.Gmax_UKI_TSB, 
                       disc_dot_product.Gmax_TSB_mod)

disc_angle_diff_Gs <- c(disc_angle_radians.Gmax_NKLS_NKBS, 
                        disc_angle_radians.Gmax_NKBS_LKI,
                        disc_angle_radians.Gmax_LKI_UKI, 
                        disc_angle_radians.Gmax_UKI_TSB, 
                        disc_angle_radians.Gmax_TSB_mod)

disc_diff_between_Gs <- as.data.frame(cbind(disc_angle_diff_Gs, disc_corr.diff_Gs))
disc_diff_between_Gs$disc_angle_diff_Gs <- as.numeric(disc_diff_between_Gs$disc_angle_diff_Gs)
disc_diff_between_Gs$disc_angle.diff_Gs.time <- disc_form_trans[-6]
disc_diff_between_Gs$disc_angle.diff_Gs.time <- factor(disc_diff_between_Gs$disc_angle.diff_Gs.time,
                                                       levels = c("NKLS to NKBS", 
                                                                  "NKBS to LKI",
                                                                  "LKI to UKI",
                                                                  "UKI to TSB",
                                                                  "TSB to modern"))

for(i in 1:nrow(disc_diff_between_Gs)){
    if(isTRUE(disc_diff_between_Gs$disc_angle_diff_Gs[i] > 90)){
        disc_diff_between_Gs$disc_angle_diff_Gs[i] <- 180 - disc_diff_between_Gs$disc_angle_diff_Gs[i]
    }
    else{
        next
    }
}

write.csv(disc_diff_between_Gs,
          "./Results/disc.differences.between.Gs.csv",
          row.names = FALSE)

###### PLOT ------
p.disc_ang_g <- ggplot(disc_diff_between_Gs) +
    geom_point(aes(x = disc_angle.diff_Gs.time, y = disc_angle_diff_Gs),
               size = 7, shape = 19) +
    scale_x_discrete(name = "Formation Transition",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Angle difference between G matrices", 
                       lim = c(0, 90)) + 
    geom_hline(yintercept = 45,
               lty = 2, col = "lightgray",
               size = 1) +
    ggtitle(expression(paste(italic("M. discors")))) +
    plot.theme

ggsave(p.disc_ang_g, 
       file = "./Results/disc.angle.g.diff.png", 
       width = 20, height = 20, units = "cm")

##### INTERMEDIA -----
### Proportion of variance in n-dimensional trait space that is explained by PC1 (i.e., the first eigenvector)
#eigen(as.matrix(G_matrix_1))$values[1]/sum(eigen(as.matrix(G_matrix_1))$values)
eigen(as.matrix(int_G_LCSB))$values[1]/sum(eigen(as.matrix(int_G_LCSB))$values) 
eigen(as.matrix(int_G_WC))$values[1]/sum(eigen(as.matrix(int_G_WC))$values) 
eigen(as.matrix(int_G_mod))$values[1]/sum(eigen(as.matrix(int_G_mod))$values) 
#close to 100% for all

### How much is the direction of Gmax (i.e., the direction first ) varying between different G-matrices? 
int_Gmax_LCSB <- eigen(int_G_LCSB)$vectors[,1]
int_Gmax_WC <- eigen(int_G_WC)$vectors[,1]
int_Gmax_mod <- eigen(int_G_mod)$vectors[,1]

# Put Gmax to norm length
int_Gmax_LCSB_norm <- f.normalize_vector(int_Gmax_LCSB)
int_Gmax_WC_norm <- f.normalize_vector(int_Gmax_WC)
int_Gmax_mod_norm <- f.normalize_vector(int_Gmax_mod)

int_dot_product.Gmax_LCSB_WC <- sum(int_Gmax_LCSB_norm * int_Gmax_WC_norm) 
# Calculate the angle in degrees
int_angle_radians.Gmax_LCSB_WC <- acos(int_dot_product.Gmax_LCSB_WC) * (180 / pi)

int_dot_product.Gmax_WC_mod <- sum(int_Gmax_WC_norm * int_Gmax_mod_norm) 
# Calculate the angle in degrees
int_angle_radians.Gmax_WC_mod <- acos(int_dot_product.Gmax_WC_mod) * (180 / pi)


int_corr.diff_Gs <- c( int_dot_product.Gmax_LCSB_WC,
                       int_dot_product.Gmax_WC_mod)

int_angle_diff_Gs <- c(int_angle_radians.Gmax_LCSB_WC,
                        int_angle_radians.Gmax_WC_mod)

int_diff_between_Gs <- as.data.frame(cbind(int_angle_diff_Gs, int_corr.diff_Gs))
int_diff_between_Gs$int_angle_diff_Gs <- as.numeric(int_diff_between_Gs$int_angle_diff_Gs)
int_diff_between_Gs$int_angle.diff_Gs.time <- int_form_trans[-3]
int_diff_between_Gs$int_angle.diff_Gs.time <- factor(int_diff_between_Gs$int_angle.diff_Gs.time,
                                                       levels = c("LCSB to WC", 
                                                                  "WC to modern"))

for(i in 1:nrow(int_diff_between_Gs)){
    if(isTRUE(int_diff_between_Gs$int_angle_diff_Gs[i] > 90)){
        int_diff_between_Gs$int_angle_diff_Gs[i] <- 180 - int_diff_between_Gs$int_angle_diff_Gs[i]
    }
    else{
        next
    }
}

write.csv(int_diff_between_Gs,
          "./Results/int.differences.between.Gs.csv",
          row.names = FALSE)

###### PLOT ------
p.int_ang_g <- ggplot(int_diff_between_Gs) +
    geom_point(aes(x = int_angle.diff_Gs.time, y = int_angle_diff_Gs),
               size = 7, shape = 19) +
    scale_x_discrete(name = "Formation Transition",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Angle difference between G matrices", 
                       lim = c(0, 90)) + 
    geom_hline(yintercept = 45,
               lty = 2, col = "lightgray",
               size = 1) +
    ggtitle(expression(paste(italic("M. intermedia")))) +
    plot.theme

ggsave(p.int_ang_g, 
       file = "./Results/int.angle.g.diff.png", 
       width = 20, height = 20, units = "cm")

##### SPECULUM -----
### Proportion of variance in n-dimensional trait space that is explained by PC1 (i.e., the first eigenvector)
#eigen(as.matrix(G_matrix_1))$values[1]/sum(eigen(as.matrix(G_matrix_1))$values)
eigen(as.matrix(spec_G_UKI))$values[1]/sum(eigen(as.matrix(spec_G_UKI))$values) 
eigen(as.matrix(spec_G_LCSB))$values[1]/sum(eigen(as.matrix(spec_G_LCSB))$values) 
eigen(as.matrix(spec_G_TSB))$values[1]/sum(eigen(as.matrix(spec_G_TSB))$values) 
eigen(as.matrix(spec_G_SHC))$values[1]/sum(eigen(as.matrix(spec_G_SHC))$values) 
eigen(as.matrix(spec_G_mod))$values[1]/sum(eigen(as.matrix(spec_G_mod))$values) 
#close to 100% for all

### How much is the direction of Gmax (i.e., the direction first ) varying between different G-matrices? 
spec_Gmax_UKI <- eigen(spec_G_UKI)$vectors[,1]
spec_Gmax_LCSB <- eigen(spec_G_LCSB)$vectors[,1]
spec_Gmax_TSB <- eigen(spec_G_TSB)$vectors[,1]
spec_Gmax_SHC <- eigen(spec_G_SHC)$vectors[,1]
spec_Gmax_mod <- eigen(spec_G_mod)$vectors[,1]

# Put Gmax to norm length
spec_Gmax_UKI_norm <- f.normalize_vector(spec_Gmax_UKI)
spec_Gmax_LCSB_norm <- f.normalize_vector(spec_Gmax_LCSB)
spec_Gmax_TSB_norm <- f.normalize_vector(spec_Gmax_TSB)
spec_Gmax_SHC_norm <- f.normalize_vector(spec_Gmax_SHC)
spec_Gmax_mod_norm <- f.normalize_vector(spec_Gmax_mod)

spec_dot_product.Gmax_UKI_LCSB <- sum(spec_Gmax_UKI_norm * spec_Gmax_LCSB_norm) 
# Calculate the angle in degrees
spec_angle_radians.Gmax_UKI_LCSB <- acos(spec_dot_product.Gmax_UKI_LCSB) * (180 / pi)

spec_dot_product.Gmax_LCSB_TSB <- sum(spec_Gmax_LCSB_norm * spec_Gmax_TSB_norm) 
# Calculate the angle in degrees
spec_angle_radians.Gmax_LCSB_TSB <- acos(spec_dot_product.Gmax_LCSB_TSB) * (180 / pi)

spec_dot_product.Gmax_TSB_SHC <- sum(spec_Gmax_TSB_norm * spec_Gmax_SHC_norm) 
# Calculate the angle in degrees
spec_angle_radians.Gmax_TSB_SHC <- acos(spec_dot_product.Gmax_TSB_SHC) * (180 / pi)

spec_dot_product.Gmax_SHC_mod <- sum(spec_Gmax_SHC_norm * spec_Gmax_mod_norm) 
# Calculate the angle in degrees
spec_angle_radians.Gmax_SHC_mod <- acos(spec_dot_product.Gmax_SHC_mod) * (180 / pi)


spec_corr.diff_Gs <- c(spec_dot_product.Gmax_UKI_LCSB,
                       spec_dot_product.Gmax_LCSB_TSB, 
                       spec_dot_product.Gmax_TSB_SHC,
                       spec_dot_product.Gmax_SHC_mod)

spec_angle_diff_Gs <- c(spec_angle_radians.Gmax_UKI_LCSB, 
                        spec_angle_radians.Gmax_LCSB_TSB, 
                        spec_angle_radians.Gmax_TSB_SHC,
                        spec_angle_radians.Gmax_SHC_mod)

spec_diff_between_Gs <- as.data.frame(cbind(spec_angle_diff_Gs, spec_corr.diff_Gs))
spec_diff_between_Gs$spec_angle_diff_Gs <- as.numeric(spec_diff_between_Gs$spec_angle_diff_Gs)
spec_diff_between_Gs$spec_angle.diff_Gs.time <- spec_form_trans[-5]
spec_diff_between_Gs$spec_angle.diff_Gs.time <- factor(spec_diff_between_Gs$spec_angle.diff_Gs.time,
                                                       levels = c("UKI to LCSB", 
                                                                  "LCSB to TSB",
                                                                  "TSB to SHC", 
                                                                  "SHC to modern"))

for(i in 1:nrow(spec_diff_between_Gs)){
    if(isTRUE(spec_diff_between_Gs$spec_angle_diff_Gs[i] > 90)){
        spec_diff_between_Gs$spec_angle_diff_Gs[i] <- 180 - spec_diff_between_Gs$spec_angle_diff_Gs[i]
    }
    else{
        next
    }
}

write.csv(spec_diff_between_Gs,
          "./Results/spec.differences.between.Gs.csv",
          row.names = FALSE)

###### PLOT ------
p.spec_ang_g <- ggplot(spec_diff_between_Gs) +
    geom_point(aes(x = spec_angle.diff_Gs.time, y = spec_angle_diff_Gs),
               size = 7, shape = 19) +
    scale_x_discrete(name = "Formation Transition",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Angle difference between G matrices", 
                       lim = c(0, 90)) + 
    geom_hline(yintercept = 45,
               lty = 2, col = "lightgray",
               size = 1) +
    ggtitle(expression(paste(italic("M. speculum")))) +
    plot.theme

ggsave(p.spec_ang_g, 
       file = "./Results/spec.angle.g.diff.png", 
       width = 20, height = 20, units = "cm")

#### ∆z DIRECTIONS OVER TIME ----

##### AGONISTES -----
### See if change is in direction of G max
## use Gmax of t1 and compare to ∆z
# Calculate the dot product of the unit vectors
agon_dot_product.Gmax_NKLS <- sum(agon_Gmax_NKLS_norm * agon_evolved_difference_unit_length_t1) 
# Calculate the angle in degrees
agon_angle_degrees.Gmax_NKLS <- acos(agon_dot_product.Gmax_NKLS) * (180 / pi)

# Calculate the dot product of the unit vectors
agon_dot_product.Gmax_NKBS <- sum(agon_Gmax_NKBS_norm * agon_evolved_difference_unit_length_t2) 
# Calculate the angle in degrees
agon_angle_degrees.Gmax_NKBS <- acos(agon_dot_product.Gmax_NKBS) * (180 / pi)

# Calculate the dot product of the unit vectors
agon_dot_product.Gmax_LKI <- sum(agon_Gmax_LKI_norm * agon_evolved_difference_unit_length_t3) 
# Calculate the angle in degrees
agon_angle_degrees.Gmax_LKI <- acos(agon_dot_product.Gmax_LKI) * (180 / pi)

# Calculate the dot product of the unit vectors
agon_dot_product.Gmax_UKI <- sum(agon_Gmax_UKI_norm * agon_evolved_difference_unit_length_t4) 
# Calculate the angle in degrees
agon_angle_degrees.Gmax_UKI <- acos(agon_dot_product.Gmax_UKI) * (180 / pi)

# Calculate the dot product of the unit vectors
agon_dot_product.Gmax_LCSB <- sum(agon_Gmax_LCSB_norm * agon_evolved_difference_unit_length_t5) 
# Calculate the angle in degrees
agon_angle_degrees.Gmax_LCSB <- acos(agon_dot_product.Gmax_LCSB) * (180 / pi)

# Calculate the dot product of the unit vectors
agon_dot_product.Gmax_TSB <- sum(agon_Gmax_TSB_norm * agon_evolved_difference_unit_length_t6) 
# Calculate the angle in degrees
agon_angle_degrees.Gmax_TSB <- acos(agon_dot_product.Gmax_TSB) * (180 / pi)

# Calculate the dot product of the unit vectors
agon_dot_product.Gmax_SHC <- sum(agon_Gmax_SHC_norm * agon_evolved_difference_unit_length_t7) 
# Calculate the angle in degrees
agon_angle_degrees.Gmax_SHC <- acos(agon_dot_product.Gmax_SHC) * (180 / pi)

# Calculate the dot product of the unit vectors
agon_dot_product.Gmax_WC <- sum(agon_Gmax_WC_norm * agon_evolved_difference_unit_length_t8) 
# Calculate the angle in degrees
agon_angle_degrees.Gmax_WC <- acos(agon_dot_product.Gmax_WC) * (180 / pi)

agon_corr.diff_Gmax_to_z <- c(agon_dot_product.Gmax_NKLS, 
                              agon_dot_product.Gmax_NKBS,
                              agon_dot_product.Gmax_LKI,
                              agon_dot_product.Gmax_UKI,
                              agon_dot_product.Gmax_LCSB,
                              agon_dot_product.Gmax_TSB,
                              agon_dot_product.Gmax_SHC,
                              agon_dot_product.Gmax_WC)

agon_angle_diff_Gmax_to_z <- c(agon_angle_degrees.Gmax_NKLS, 
                               agon_angle_degrees.Gmax_NKBS,
                               agon_angle_degrees.Gmax_LKI,
                               agon_angle_degrees.Gmax_UKI,
                               agon_angle_degrees.Gmax_LCSB,
                               agon_angle_degrees.Gmax_TSB,
                               agon_angle_degrees.Gmax_SHC,
                               agon_angle_degrees.Gmax_WC)

agon_diff_between_Gmax_z <- as.data.frame(cbind(agon_angle_diff_Gmax_to_z, agon_corr.diff_Gmax_to_z))
agon_diff_between_Gmax_z$agon_angle.diff.time <- agon_form_trans[-9]
agon_diff_between_Gmax_z$agon_angle.diff.time <- factor(agon_diff_between_Gmax_z$agon_angle.diff.time,
                                                        levels = c("NKLS to NKBS", 
                                                                   "NKBS to LKI",
                                                                   "LKI to UKI",
                                                                   "UKI to LCSB", 
                                                                   "LCSB to TSB",
                                                                   "TSB to SHC", 
                                                                   "SHC to WC",
                                                                   "WC to modern"))

colnames(agon_diff_between_Gmax_z) <- c("agon_angle_diff_Gmax_to_z", "agon_corr.diff_Gmax_to_z", "agon_angle.diff.time")
agon_diff_between_Gmax_z$agon_angle_diff_Gmax_to_z <- as.numeric(agon_diff_between_Gmax_z$agon_angle_diff_Gmax_to_z)

for(i in 1:nrow(agon_diff_between_Gmax_z)){
    if(isTRUE(agon_diff_between_Gmax_z$agon_angle_diff_Gmax_to_z[i] > 90)){
        agon_diff_between_Gmax_z$agon_angle_diff_Gmax_to_z[i] <- 180 - as.numeric(agon_diff_between_Gmax_z$agon_angle_diff_Gmax_to_z[i])
    }
    else{
        next
    }
}

write.csv(agon_diff_between_Gmax_z,
          "./Results/agon.differences.between.Gmax.z.csv",
          row.names = FALSE)

###### PLOT ------
p.agon.g.pheno <- ggplot(agon_diff_between_Gmax_z[-9,]) +
    geom_point(aes(x = agon_angle.diff.time, y = agon_angle_diff_Gmax_to_z),
               size = 5, shape = 17) +
    scale_x_discrete(name = "Formation",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Angle difference between G matrices", 
                       lim = c(0, 90)) +
    geom_hline(yintercept = 45, 
               color = "lightgray", lty = 2, size = 1) +
    ggtitle(expression(paste(italic("M. agonistes")))) +
    plot.theme

ggsave(p.agon.g.pheno, 
       file = "./Results/agon.angle.g.pheno.png", 
       width = 14, height = 12, units = "cm")

##### DISCORS -----
### See if change is in direction of G max
## use Gmax of t1 and compare to ∆z
# Calculate the dot product of the unit vectors
disc_dot_product.Gmax_NKLS <- sum(disc_Gmax_NKLS_norm * disc_evolved_difference_unit_length_t1) 
# Calculate the angle in degrees
disc_angle_degrees.Gmax_NKLS <- acos(disc_dot_product.Gmax_NKLS) * (180 / pi)

# Calculate the dot product of the unit vectors
disc_dot_product.Gmax_NKBS <- sum(disc_Gmax_NKBS_norm * disc_evolved_difference_unit_length_t2) 
# Calculate the angle in degrees
disc_angle_degrees.Gmax_NKBS <- acos(disc_dot_product.Gmax_NKBS) * (180 / pi)

# Calculate the dot product of the unit vectors
disc_dot_product.Gmax_LKI <- sum(disc_Gmax_LKI_norm * disc_evolved_difference_unit_length_t3) 
# Calculate the angle in degrees
disc_angle_degrees.Gmax_LKI <- acos(disc_dot_product.Gmax_LKI) * (180 / pi)

# Calculate the dot product of the unit vectors
disc_dot_product.Gmax_UKI <- sum(disc_Gmax_UKI_norm * disc_evolved_difference_unit_length_t4) 
# Calculate the angle in degrees
disc_angle_degrees.Gmax_UKI <- acos(disc_dot_product.Gmax_UKI) * (180 / pi)

# Calculate the dot product of the unit vectors
disc_dot_product.Gmax_TSB <- sum(disc_Gmax_TSB_norm * disc_evolved_difference_unit_length_t5) 
# Calculate the angle in degrees
disc_angle_degrees.Gmax_TSB <- acos(disc_dot_product.Gmax_TSB) * (180 / pi)

disc_corr.diff_Gmax_to_z <- c(disc_dot_product.Gmax_NKLS, 
                              disc_dot_product.Gmax_NKBS,
                              disc_dot_product.Gmax_LKI,
                              disc_dot_product.Gmax_UKI,
                              disc_dot_product.Gmax_TSB)

disc_angle_diff_Gmax_to_z <- c(disc_angle_degrees.Gmax_NKLS, 
                               disc_angle_degrees.Gmax_NKBS,
                               disc_angle_degrees.Gmax_LKI,
                               disc_angle_degrees.Gmax_UKI,
                               disc_angle_degrees.Gmax_TSB)

disc_diff_between_Gmax_z <- as.data.frame(cbind(disc_angle_diff_Gmax_to_z, disc_corr.diff_Gmax_to_z))
disc_diff_between_Gmax_z$disc_angle.diff.time <- disc_form_trans[-6]
disc_diff_between_Gmax_z$disc_angle.diff.time <- factor(disc_diff_between_Gmax_z$disc_angle.diff.time,
                                                        levels = c("NKLS to NKBS", 
                                                                   "NKBS to LKI",
                                                                   "LKI to UKI",
                                                                   "UKI to TSB",
                                                                   "TSB to modern"))

colnames(disc_diff_between_Gmax_z) <- c("disc_angle_diff_Gmax_to_z", "disc_corr.diff_Gmax_to_z", "disc_angle.diff.time")
disc_diff_between_Gmax_z$disc_angle_diff_Gmax_to_z <- as.numeric(disc_diff_between_Gmax_z$disc_angle_diff_Gmax_to_z)

for(i in 1:nrow(disc_diff_between_Gmax_z)){
    if(isTRUE(disc_diff_between_Gmax_z$disc_angle_diff_Gmax_to_z[i] > 90)){
        disc_diff_between_Gmax_z$disc_angle_diff_Gmax_to_z[i] <- 180 - as.numeric(disc_diff_between_Gmax_z$disc_angle_diff_Gmax_to_z[i])
    }
    else{
        next
    }
}

write.csv(disc_diff_between_Gmax_z,
          "./Results/disc.differences.between.Gmax.z.csv",
          row.names = FALSE)

###### PLOT ------
p.disc.g.pheno <- ggplot(disc_diff_between_Gmax_z[-6,]) +
    geom_point(aes(x = disc_angle.diff.time, y = disc_angle_diff_Gmax_to_z),
               size = 5, shape = 17) +
    scale_x_discrete(name = "Formation",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Angle difference between G matrices", 
                       lim = c(0, 90)) +
    geom_hline(yintercept = 45, 
               color = "lightgray", lty = 2, size = 1) +
    ggtitle(expression(paste(italic("M. discors")))) +
    plot.theme

ggsave(p.disc.g.pheno, 
       file = "./Results/disc.angle.g.pheno.png", 
       width = 14, height = 12, units = "cm")

##### INTERMEDIA -----
### See if change is in direction of G max
## use Gmax of t1 and compare to ∆z
# Calculate the dot product of the unit vectors
int_dot_product.Gmax_LCSB <- sum(int_Gmax_LCSB_norm * int_evolved_difference_unit_length_t1) 
# Calculate the angle in degrees
int_angle_degrees.Gmax_LCSB <- acos(int_dot_product.Gmax_LCSB) * (180 / pi)

# Calculate the dot product of the unit vectors
int_dot_product.Gmax_WC <- sum(int_Gmax_WC_norm * int_evolved_difference_unit_length_t2) 
# Calculate the angle in degrees
int_angle_degrees.Gmax_WC <- acos(int_dot_product.Gmax_WC) * (180 / pi)

int_corr.diff_Gmax_to_z <- c(int_dot_product.Gmax_LCSB,
                             int_dot_product.Gmax_WC)

int_angle_diff_Gmax_to_z <- c(int_angle_degrees.Gmax_LCSB,
                              int_angle_degrees.Gmax_WC)

int_diff_between_Gmax_z <- as.data.frame(cbind(int_angle_diff_Gmax_to_z, int_corr.diff_Gmax_to_z))
int_diff_between_Gmax_z$int_angle.diff.time <- int_form_trans[-3]
int_diff_between_Gmax_z$int_angle.diff.time <- factor(int_diff_between_Gmax_z$int_angle.diff.time,
                                                        levels = c("LCSB to WC",
                                                                   "WC to modern"))

colnames(int_diff_between_Gmax_z) <- c("int_angle_diff_Gmax_to_z", "int_corr.diff_Gmax_to_z", "int_angle.diff.time")
int_diff_between_Gmax_z$int_angle_diff_Gmax_to_z <- as.numeric(int_diff_between_Gmax_z$int_angle_diff_Gmax_to_z)

for(i in 1:nrow(int_diff_between_Gmax_z)){
    if(isTRUE(int_diff_between_Gmax_z$int_angle_diff_Gmax_to_z[i] > 90)){
        int_diff_between_Gmax_z$int_angle_diff_Gmax_to_z[i] <- 180 - as.numeric(int_diff_between_Gmax_z$int_angle_diff_Gmax_to_z[i])
    }
    else{
        next
    }
}

write.csv(int_diff_between_Gmax_z,
          "./Results/int.differences.between.Gmax.z.csv",
          row.names = FALSE)

###### PLOT ------
p.int.g.pheno <- ggplot(int_diff_between_Gmax_z[-3,]) +
    geom_point(aes(x = int_angle.diff.time, y = int_angle_diff_Gmax_to_z),
               size = 5, shape = 17) +
    scale_x_discrete(name = "Formation",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Angle difference between G matrices", 
                       lim = c(0, 90)) +
    geom_hline(yintercept = 45, 
               color = "lightgray", lty = 2, size = 1) +
    ggtitle(expression(paste(italic("M. intermedia")))) +
    plot.theme

ggsave(p.int.g.pheno, 
       file = "./Results/int.angle.g.pheno.png", 
       width = 14, height = 12, units = "cm")

##### SPECULUM -----
### See if change is in direction of G max
## use Gmax of t1 and compare to ∆z
# Calculate the dot product of the unit vectors
spec_dot_product.Gmax_UKI <- sum(spec_Gmax_UKI_norm * spec_evolved_difference_unit_length_t1) 
# Calculate the angle in degrees
spec_angle_degrees.Gmax_UKI <- acos(spec_dot_product.Gmax_UKI) * (180 / pi)

# Calculate the dot product of the unit vectors
spec_dot_product.Gmax_LCSB <- sum(spec_Gmax_LCSB_norm * spec_evolved_difference_unit_length_t2) 
# Calculate the angle in degrees
spec_angle_degrees.Gmax_LCSB <- acos(spec_dot_product.Gmax_LCSB) * (180 / pi)

# Calculate the dot product of the unit vectors
spec_dot_product.Gmax_TSB <- sum(spec_Gmax_TSB_norm * spec_evolved_difference_unit_length_t3) 
# Calculate the angle in degrees
spec_angle_degrees.Gmax_TSB <- acos(spec_dot_product.Gmax_TSB) * (180 / pi)

# Calculate the dot product of the unit vectors
spec_dot_product.Gmax_SHC <- sum(spec_Gmax_SHC_norm * spec_evolved_difference_unit_length_t4) 
# Calculate the angle in degrees
spec_angle_degrees.Gmax_SHC <- acos(spec_dot_product.Gmax_SHC) * (180 / pi)

spec_corr.diff_Gmax_to_z <- c(spec_dot_product.Gmax_UKI,
                              spec_dot_product.Gmax_LCSB,
                              spec_dot_product.Gmax_TSB,
                              spec_dot_product.Gmax_SHC)

spec_angle_diff_Gmax_to_z <- c(spec_angle_degrees.Gmax_UKI,
                               spec_angle_degrees.Gmax_LCSB,
                               spec_angle_degrees.Gmax_TSB,
                               spec_angle_degrees.Gmax_SHC)

spec_diff_between_Gmax_z <- as.data.frame(cbind(spec_angle_diff_Gmax_to_z, spec_corr.diff_Gmax_to_z))
spec_diff_between_Gmax_z$spec_angle.diff.time <- spec_form_trans[-5]
spec_diff_between_Gmax_z$spec_angle.diff.time <- factor(spec_diff_between_Gmax_z$spec_angle.diff.time,
                                                        levels = c("UKI to LCSB", 
                                                                   "LCSB to TSB",
                                                                   "TSB to SHC", 
                                                                   "SHC to modern"))

colnames(spec_diff_between_Gmax_z) <- c("spec_angle_diff_Gmax_to_z", "spec_corr.diff_Gmax_to_z", "spec_angle.diff.time")
spec_diff_between_Gmax_z$spec_angle_diff_Gmax_to_z <- as.numeric(spec_diff_between_Gmax_z$spec_angle_diff_Gmax_to_z)

for(i in 1:nrow(spec_diff_between_Gmax_z)){
    if(isTRUE(spec_diff_between_Gmax_z$spec_angle_diff_Gmax_to_z[i] > 90)){
        spec_diff_between_Gmax_z$spec_angle_diff_Gmax_to_z[i] <- 180 - as.numeric(spec_diff_between_Gmax_z$spec_angle_diff_Gmax_to_z[i])
    }
    else{
        next
    }
}

write.csv(spec_diff_between_Gmax_z,
          "./Results/spec.differences.between.Gmax.z.csv",
          row.names = FALSE)

###### PLOT ------
p.spec.g.pheno <- ggplot(spec_diff_between_Gmax_z[-5,]) +
    geom_point(aes(x = spec_angle.diff.time, y = spec_angle_diff_Gmax_to_z),
               size = 5, shape = 17) +
    scale_x_discrete(name = "Formation",
                     guide = guide_axis(angle = 45)) +
    scale_y_continuous(name = "Angle difference between G matrices", 
                       lim = c(0, 90)) +
    geom_hline(yintercept = 45, 
               color = "lightgray", lty = 2, size = 1) +
    ggtitle(expression(paste(italic("M. speculum")))) +
    plot.theme

ggsave(p.spec.g.pheno, 
       file = "./Results/spec.angle.g.pheno.png", 
       width = 14, height = 12, units = "cm")

#### ANCESTRAL G ----
#using discors, look at both evolvability from t1 to t2 and t1 to modern
#disc_G_NKLS 

##### AGONISTES -----
#agon_evolved_difference_unit_length_t1

agon_disc_observed_evolvability_in_direction_of_change_t1 <- t(agon_evolved_difference_unit_length_t1)%*%as.matrix(disc_G_NKLS)%*%agon_evolved_difference_unit_length_t1
agon_disc_observed_evolvability_in_direction_of_change_t1 >= disc_sum_t1$Averages[[1]]

##### INTERMEDIA -----
#int_evolved_difference_unit_length_t1

int_disc_observed_evolvability_in_direction_of_change_t1 <- t(int_evolved_difference_unit_length_t1)%*%as.matrix(disc_G_NKLS)%*%int_evolved_difference_unit_length_t1
int_disc_observed_evolvability_in_direction_of_change_t1 >= disc_sum_t1$Averages[[1]]

##### SPECULUM -----
#spec_evolved_difference_unit_length_t1

spec_disc_observed_evolvability_in_direction_of_change_t1 <- t(spec_evolved_difference_unit_length_t1)%*%as.matrix(disc_G_NKLS)%*%spec_evolved_difference_unit_length_t1
spec_disc_observed_evolvability_in_direction_of_change_t1 >= disc_sum_t1$Averages[[1]]

#### ∆z DIRECTIONS COMPARED TO ANCESTRAL G ----

##### AGONISTES -----
# Calculate the dot product of the unit vectors
agon_dot_product.Gmax_ancestral <- sum(disc_Gmax_NKLS_norm * agon_evolved_difference_unit_length_t1) #-0.9743072
# Calculate the angle in degrees
agon_angle_degrees.Gmax_ancestral <- acos(agon_dot_product.Gmax_ancestral) * (180 / pi)
180-agon_angle_degrees.Gmax_ancestral #13.01599

##### INTERMEDIA -----
# Calculate the dot product of the unit vectors
int_dot_product.Gmax_ancestral <- sum(disc_Gmax_NKLS_norm * int_evolved_difference_unit_length_t1) #0.443308
# Calculate the angle in degrees
int_angle_degrees.Gmax_ancestral <- acos(int_dot_product.Gmax_ancestral) * (180 / pi)
int_angle_degrees.Gmax_ancestral #63.68486

##### SPECULUM -----
# Calculate the dot product of the unit vectors
spec_dot_product.Gmax_ancestral <- sum(disc_Gmax_NKLS_norm * spec_evolved_difference_unit_length_t1) #-0.9777634
# Calculate the angle in degrees
spec_angle_degrees.Gmax_ancestral <- acos(spec_dot_product.Gmax_ancestral) * (180 / pi)
180-spec_angle_degrees.Gmax_ancestral #12.10541


