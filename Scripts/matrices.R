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
agon.meta.form$zooid.id <- 
agon.zooid.list <- unique(agon.meta.form$ID)
length(zooid_list) #6264 (6658 for 3 zoo)

colony_list <- unique(df$colony.id)
length(colony_list) #599 (711 for 3 zoo)

# arrange formations from oldest to youngest
df$formation <- factor(df$formation, levels = c("NKLS", "NKBS", "Tewkesbury", 
                                                "Upper Kai-Iwi",  "Tainui", 
                                                "SHCSBSB", "modern")) 
formation_list <- unique(df$formation)
length(formation_list) #7

#same order as in df
names(df)
traits = names(df[, c("ln.zh", "ln.mpw.b", "ln.cw.m", "ln.cw.d", 
                      "ln.ow.m", "ln.oh", "ln.c.side", "ln.o.side")])
length(traits) #8

##### TRIM DATASET ----
df.trim <- df %>%
    dplyr::select(zooid.id, colony.id, locality, formation, matches(traits))

colNums <- match(c(traits, "zooid.id"), names(df.trim))
#  4  5 6  7  8  9 10 11  1 (i.e., 4:11 are traits of interest)

df = as.data.frame(df.trim)

#### MEANS ----
mean_by_formation_colony = df %>% #use this going forward
    dplyr::group_by(formation, colony.id) %>%
    dplyr::summarize(n.zooid = length(zooid.id),
                     
                     avg.zh = mean(ln.zh, na.rm = T),
                     sd.zh = sd(ln.zh, na.rm = T),
                     
                     avg.mpw.b = mean(ln.mpw.b, na.rm = T),
                     sd.mpw.b = sd(ln.mpw.b, na.rm = T),
                     
                     avg.cw.m = mean(ln.cw.m, na.rm = T),
                     sd.cw.m = sd(ln.cw.m, na.rm = T),
                     
                     avg.cw.d = mean(ln.cw.d, na.rm = T),
                     sd.cw.d = sd(ln.cw.d, na.rm = T),
                     
                     avg.ow.m = mean(ln.ow.m, na.rm = T),
                     sd.ow.m = sd(ln.ow.m, na.rm = T),
                     
                     avg.oh = mean(ln.oh, na.rm = T),
                     sd.oh = sd(ln.oh, na.rm = T),
                     
                     avg.o.side = mean(ln.o.side, na.rm = T),
                     sd.o.side = sd(ln.o.side, na.rm = T),
                     
                     avg.c.side = mean(ln.c.side, na.rm = T),
                     sd.c.side = sd(ln.c.side, na.rm = T)) %>%
    as.data.frame()
min(mean_by_formation_colony$n.zooid) #3

mean_by_formation = df %>%
    dplyr::group_by(formation) %>%
    dplyr::summarize(num.col = length(unique(colony.id)),
                     num.zooid = length(unique(zooid.id)),
                     avg.zooid = ceiling(num.zooid/num.col), #round up to nearest integer
                     
                     min.zh = min(ln.zh, na.rm = T),
                     max.zh = max(ln.zh, na.rm = T),
                     avg.zh = mean(ln.zh, na.rm = T),
                     sd.zh = sd(ln.zh, na.rm = T),
                     var.zh = var(ln.zh, na.rm = T),
                     
                     min.mpw.b = min(ln.mpw.b, na.rm = T),
                     max.mpw.b = max(ln.mpw.b, na.rm = T),
                     avg.mpw.b = mean(ln.mpw.b, na.rm = T),
                     sd.mpw.b = sd(ln.mpw.b, na.rm = T),
                     var.mpw.b = var(ln.mpw.b, na.rm = T),
                     
                     min.cw.m = min(ln.cw.m, na.rm = T),
                     max.cw.m = max(ln.cw.m, na.rm = T),
                     avg.cw.m = mean(ln.cw.m, na.rm = T),
                     sd.cw.m = sd(ln.cw.m, na.rm = T),
                     var.cw.m = var(ln.cw.m, na.rm = T),
                     
                     min.cw.d = min(ln.cw.d, na.rm = T),
                     max.cw.d = max(ln.cw.d, na.rm = T),
                     avg.cw.d = mean(ln.cw.d, na.rm = T),
                     sd.cw.d = sd(ln.cw.d, na.rm = T),
                     var.cw.d = var(ln.cw.d, na.rm = T),
                     
                     min.ow.m = min(ln.ow.m, na.rm = T),
                     max.ow.m = max(ln.ow.m, na.rm = T),
                     avg.ow.m = mean(ln.ow.m, na.rm = T),
                     sd.ow.m = sd(ln.ow.m, na.rm = T),
                     var.ow.m = var(ln.ow.m, na.rm = T),
                     
                     min.oh = min(ln.oh, na.rm = T),
                     max.oh = max(ln.oh, na.rm = T),
                     avg.oh = mean(ln.oh, na.rm = T),
                     sd.oh = sd(ln.oh, na.rm = T),
                     var.oh = var(ln.oh, na.rm = T),
                     
                     min.o.side = min(ln.o.side, na.rm = T),
                     max.o.side = max(ln.o.side, na.rm = T),
                     avg.o.side = mean(ln.o.side, na.rm = T),
                     sd.o.side = sd(ln.o.side, na.rm = T),
                     var.o.side = var(ln.o.side, na.rm = T),
                     
                     min.c.side = min(ln.c.side, na.rm = T),
                     max.c.side = max(ln.c.side, na.rm = T),
                     avg.c.side = mean(ln.c.side, na.rm = T),
                     sd.c.side = sd(ln.c.side, na.rm = T),
                     var.c.side = var(ln.c.side, na.rm = T)) %>%
    as.data.frame()
## Grabowski & Porto claim sampling of 60 per sp...
#NKBS has heighest variance in traits because high sample size
write.csv(mean_by_formation,
          "Results/mean.per.formation.csv",
          row.names = FALSE)

colony_means = df %>%
    dplyr::group_by(colony.id) %>%
    dplyr::summarize(formation = formation[1],
                     n.zooid = length(unique(zooid.id)),
                     avg.zh = mean(ln.zh, na.rm = T),
                     avg.mpw.b = mean(ln.mpw.b, na.rm = T),
                     avg.cw.m = mean(ln.cw.m, na.rm = T),
                     avg.cw.d = mean(ln.cw.d, na.rm = T),
                     avg.ow.m = mean(ln.ow.m, na.rm = T),
                     avg.oh = mean(ln.oh, na.rm = T),
                     avg.o.side = mean(ln.o.side, na.rm = T),
                     avg.c.side = mean(ln.c.side, na.rm = T)) %>%
    as.data.frame()

means = df %>%
    dplyr::summarize(avg.zh = mean(ln.zh, na.rm = T),
                     avg.mpw.b = mean(ln.mpw.b, na.rm = T),
                     avg.cw.m = mean(ln.cw.m, na.rm = T),
                     avg.cw.d = mean(ln.cw.d, na.rm = T),
                     avg.ow.m = mean(ln.ow.m, na.rm = T),
                     avg.oh = mean(ln.oh, na.rm = T),
                     avg.o.side = mean(ln.o.side, na.rm = T),
                     avg.c.side = mean(ln.c.side, na.rm = T)) %>%
    as.data.frame()

sum.data.list = list(mean_by_formation, mean_by_formation_colony, means)
save(sum.data.list,
     file = "./Results/sum.data.list.RData")
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
