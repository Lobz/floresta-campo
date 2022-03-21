## CONSTANTS & UTILS

finalstep <- 2000
samplesize <- 100
numreps <- 1
chunksize <- 1

gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
source("headless/createxml.utils.R")


## filenaming
groupname <-  gen_groupname("LHS")

## parametrizing

par_names <- c("wildfire_rate",
                "araucaria_fire_tolerance", 
                "shade_threshold_ratio", 
                "tree_dispersal", 
                "grass_flammability")

q.arg <- list(list(min=0.00,max=0.3), 
                list(min=0.8,max=1.0), 
                list(min=1.0,max=3.0), 
                list(min=5,max=30), 
                list(min=0.55,0.70))


## creating parameter data.frame
library(pse)
my_LHS_pars <- LHS(model=NULL,
                  factors=par_names,
                  N=samplesize,
                  q.arg=q.arg,
                  repetitions=numreps)

mylhsfilename <- paste0("data/",groupname,".RData")
save(my_LHS_pars,groupname,file=mylhsfilename)

par.data <- my_LHS_pars$data
par.data$par_group <- rownames(par.data)
## WRITING
my_filenames <- createxml(par.data, groupname, stop.at.area.limit=FALSE)
## RUNNING
outputdir <- paste0('headless_outputs/',groupname,'-out')
run_simulations(my_filenames, outputdir)
