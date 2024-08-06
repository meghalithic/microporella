#reading in file
df <- read.csv("~/Desktop/Vouchers.csv",
               header = TRUE)

head(df)

df$Bleached[df$BLEED_no == 1114]

unique(df$Bleached[df$BLEED_no == 1114])

yes <- c("yes", "YES", "Yes")
nrow(df[df$BLEED_no == 1114 & df$Bleached %in% yes,])

df$Name_of_file[df$BLEED_no == 1114 & df$Bleached %in% yes]

### DISCORS
m.discors.nums <- c(1114, 420) #put in the numbers from your notes
m.discors.img.names <- c() #rename to species 
for(i in 1:length(m.discors.nums)){
 m.discors.img.names <-  c(m.discors.img.names, df$Name_of_file[df$BLEED_no == m.discors.nums[i] & df$Bleached %in% yes])
}

##SPECULUM
m.speculum.nums <- c(1114, 420) #put in the numbers from your notes
m.speculum.img.names <- c() #rename to species 
for(i in 1:length(m.speculum.nums)){
  m.speculum.img.names <-  c(m.speculum.img.names, df$Name_of_file[df$BLEED_no == m.speculum.nums[i] & df$Bleached %in% yes])
}

##INTERMEDIA
m.intermedia.nums <- c(1114, 420) #put in the numbers from your notes
m.intermedia.img.names <- c() #rename to species 
for(i in 1:length(m.intermedia.nums)){
  m.intermedia.img.names <-  c(m.intermedia.img.names, df$Name_of_file[df$BLEED_no == m.intermedia.nums[i] & df$Bleached %in% yes])
}

##AGONISTES
m.agonistes.nums <- c(1114, 420) #put in the numbers from your notes
m.agonistes.img.names <- c() #rename to species 
for(i in 1:length(m.agonistes.nums)){
  m.agonistes.img.names <-  c(m.agonistes.img.names, df$Name_of_file[df$BLEED_no == m.agonistes.nums[i] & df$Bleached %in% yes])
}

## STEGINOPORELLA MAGNIFICA
s.magnifica.nums <- c(1114, 420) #put in the numbers from your notes
s.magnifica.img.names <- c() #rename to species 
for(i in 1:length(s.magnifica.nums)){
  s.magnifica.img.names <-  c(s.magnifica.img.names, df$Name_of_file[df$BLEED_no == s.magnifica.nums[i] & df$Bleached %in% yes])
}

##GET READY TO WRITE IT OUT
m.disc.sp <- rep("M. discors", length(m.discors.nums))
m.discors <- cbind(m.disc.sp, m.discors.img.names)

m.spec.sp <- rep("M. speculum", length(m.speculum.nums))
m.speculum <- cbind(m.spec.sp, m.speculum.img.names)

m.int.sp <- rep("M. intermedia", length(m.intermedia.nums))
m.intermedia <- cbind(m.int.sp, m.intermedia.img.names)

m.ago.sp <- rep("M. agonistes", length(m.agonistes.nums))
m.agonistes <- cbind(m.ago.sp, m.agonistes.img.names)

s.mag.sp <- rep("S. magnifica", length(s.magnifica.nums))
s.magnifica <- cbind(s.mag.sp, s.magnifica.img.names)

colnames(m.discors) <- c("species", "img.name")
colnames(m.speculum) <- c("species", "img.name")
colnames(m.intermedia) <- c("species", "img.name")
colnames(m.agonistes) <- c("species", "img.name")
colnames(s.magnifica) <- c("species", "img.name")

##WRITE IT OUT
img.names <- rbind(m.discors, m.speculum, m.intermedia, m.agonistes, s.magnifica)

write.csv(img.names,
          "img.names.csv",
          row.names = FALSE)
