## CONSTANTS & UTILS

finalstep <- 1000
samplesize <- 500
numreps <- 1
chunksize <- 25

gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
source("./headless/createxml.utils.R")

## filenaming
groupname <-  gen_groupname("explore")

## PARAMETERS
n <- numreps*samplesize
par.data <- data.frame(
            grass_flammability=rep(seq(0.4,0.7,length.out=samplesize),numreps),
            wildfire_rate=rep(0.1,n),
            initial_pop_total=rep(0,n),
            par_group=1:n
            )

# par.data <- data.frame(
#     wildfire_rate=0.1,
#     shade_threshold_ratio=2,
#     araucaria_fire_tolerance=0.9, 
#     tree_dispersal=10, 
#     grass_flammability=0.6)

# par.data$par_group <- 1
## WRITING
my_filenames <- createxml(par.data,groupname,scenarios=FALSE, stop.at.extinction=FALSE)
## RUNNING
outputdir <- paste0('headless_outputs/',groupname,'-out')
run_simulations(my_filenames, outputdir)
