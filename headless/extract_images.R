
## extract image files
imagedir <- "./data/scenarios_Dec232021_kv20u-out/scenarios_Dec232021_kv20u-out/snapshot/"
fullimages<- as.character(subset(data,full & !is.na(image_file))$image_file)
nofiimages<- as.character(subset(data,noFi & !is.na(image_file))$image_file)
noarimages<- as.character(subset(data,noAr & !is.na(image_file))$image_file)

file.rename(paste0(imagedir,fullimages),paste0(imagedir,'/full/',fullimages))
file.rename(paste0(imagedir,nofiimages),paste0(imagedir,'/nofi/',nofiimages))
file.rename(paste0(imagedir,noarimages),paste0(imagedir,'/noar/',noarimages))
