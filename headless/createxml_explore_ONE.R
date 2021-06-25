## CONSTANTS & UTILS

finalstep <- 2000
samplesize <- 30
numreps <- 3
chunksize <- 10

gamlfile <- '..\\..\\FireandForest\\models\\instafire.gaml'
source("createxml.utils.R")

## filenaming
rnd <- paste0(sample(chars, 5, TRUE),collapse="")
today <- paste0(strsplit(date()," ")[[1]][c(2:3,5)],collapse="")
groupname <-  paste0("LHS_",today,rnd)


## PARAMETERS
grass_flammability <- rep(seq(0.4,0.7,length.out=samplesize),numreps)
