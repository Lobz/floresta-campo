## CONSTANTS & UTILS

finalstep <- 2000
samplesize <- 100
numreps <- 1
chunksize <- 10

gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
source("./headless/createxml.utils.R")

## filenaming
groupname <-  gen_groupname("LHS")

## parametrizing

par_names <- c("wildfire_rate",
                "shade_threshold_ratio", 
                "araucaria_fire_tolerance", 
                "tree_dispersal", 
                "grass_flammability")

q.arg <- list(list(min=0.05,max=0.15), 
                list(min=1.5,max=2.5), 
                list(min=0.8,max=1.0), 
                list(min=5,max=15), 
                list(min=0.55,0.65))


## creating parameter data.frame
library(pse)
my_LHS_pars <- LHS(model=NULL,
                  factors=par_names,
                  N=samplesize,
                  q.arg=q.arg,
                  repetitions=numreps)

mylhsfilename<- paste0("data/",groupname,".RData")
save(my_LHS_pars,groupname,file=mylhsfilename)

par.data <- my_LHS_pars$data
par.data$par_group <- rownames(par.data)
## WRITING
my_filenames <- createxml(par.data,groupname)
## RUNNING
outputdir <- paste0('headless_outputs/',groupname,'-out')
run_simulations(my_filenames, outputdir)

