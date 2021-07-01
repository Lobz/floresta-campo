## CONSTANTS & UTILS

finalstep <- 1000
samplesize <- 500
numreps <- 1
chunksize <- 25

gamlfile <- '..\\..\\FireandForest\\models\\instafire.gaml'
source("createxml.utils.R")

## filenaming
rnd <- paste0(sample(chars, 5, TRUE),collapse="")
today <- paste0(strsplit(date()," ")[[1]][c(2:3,5)],collapse="")
groupname <-  paste0("greass_flammability_",rnd)

## PARAMETERS
n <- numreps*samplesize
par.data <- data.frame(
            grass_flammability=rep(seq(0.4,0.7,length.out=samplesize),numreps),
            wildfire_rate=rep(0.1,n),
            initial_pop_total=rep(0,n),
            par_group=1:n
            )
## WRITING
my_filenames <- createxml(par.data,groupname,scenarios=FALSE, stop.at.extinction=FALSE)
## RUNNING
outputdir <- paste0('headless_outputs/',groupname,'-out')
run_simulations(my_filenames, outputdir)
