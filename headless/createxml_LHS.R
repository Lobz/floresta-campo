## CONSTANTS & UTILS

finalstep <- 20
samplesize <- 40
numreps <- 1
chunksize <- 10

gamlfile <- '..\\..\\FireandForest\\models\\instafire.gaml'
source("createxml.utils.R")

## filenaming
rnd <- paste0(sample(chars, 5, TRUE),collapse="")
today <- paste0(strsplit(date()," ")[[1]][c(2:3,5)],collapse="")
groupname <-  paste0("LHS_",today,"_",rnd)

## parametrizing

par_names <- c("wildfire_rate",
                "shade_threshold_ratio", 
                "araucaria_fire_tolerance", 
                "tree_dispersal", 
                "grass_flammability")

q.arg <- list(list(min=0.00,max=0.3), 
                list(min=1.0,max=3.0), 
                list(min=0.5,max=1.0), 
                list(min=5,max=30), 
                list(min=0.55,0.7))


## creating parameter data.frame
library(pse)
my_LHS_pars <- LHS(model=NULL,
                  factors=par_names,
                  N=samplesize,
                  q.arg=q.arg,
                  repetitions=numreps)

save(my_LHS_pars,groupname,file=paste0("data/",groupname,".RData"))

par.data <- my_LHS_pars$data
par.data$par_group <- rownames(par.data)
## WRITING
my_filenames <- createxml(par.data,groupname)
## RUNNING
outputdir <- paste0('headless_outputs/',groupname,'-out')
run_simulations(my_filenames, outputdir)

