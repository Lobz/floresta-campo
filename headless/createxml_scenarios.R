## CONSTANTS & UTILS

finalstep <- 200
samplesize <- 1
numreps <- 1
chunksize <- 10

gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
source("./headless/createxml.utils.R")

## filenaming
groupname <-  gen_groupname("scenarios")

## parametrizing

par.data <- data.frame(
    wildfire_rate=0.1,
    shade_threshold_ratio=2,
    araucaria_fire_tolerance=0.9, 
    tree_dispersal=10, 
    grass_flammability=0.6)

par.data$par_group <- 1
## WRITING
my_filenames <- createxml(par.data, groupname, numreps=numreps, stop.at.area.limit=F, graphics=TRUE, graphics_framerate=50)
## RUNNING
outputdir <- paste0('headless_outputs/',groupname,'-out')
run_simulations(my_filenames, outputdir)
