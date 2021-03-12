
## filenaming
today <- paste0(strsplit(date()," ")[[1]][c(2:3,5)],collapse="")
gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
my_filename <- paste0("LHS_",today,".xml")

## parametrizing
finalstep <- 5
samplesize <- 40
numreps <- 3

par_names <- c("wildfire_rate", "shade_threshold_araucaria","shade_threshold_ratio","araucaria_base_flammability","araucaria_dispersal","broadleaf_dispersal")

q.arg <- list(list(min=0.05,max=0.2),list(min=0.5,max=1.5),list(min=1.5,max=2.5),list(min=0.5,max=0.9),list(min=5,max=30),list(min=5,max=30))


## creating parameter data.frame
library(pse)
my_LHS_pars <- LHS(model=NULL,
                  factors=par_names,
                  N=samplesize,
                  q.arg=q.arg,
                  repetitions=numreps)

save(my_LHS_pars,file=paste0(my_filename,".RData"))

par.data <- my_LHS_pars$data
par.data$par_group <- rownames(par.data)

## CONSTANTS & UTILS
source("createxml.utils.R")

### Parameters
sim.params<-apply(par.data,1,par.row)
## scenarios
FMparams <- paste0(sim.params,'')
NAparams <- paste0(sim.params,'<Parameter name="initial_pop_ratio" type="INT" value="0" />')
NFparams <- paste0(sim.params,'<Parameter name="Wildfires" type="BOOLEAN" value="false" />')

my_names <- c("Full","NoAr","NoFi")
sim.params <- c(FMparams,NAparams,NFparams)

## UNIQUE IDS
sim_ids <- paste0('LHS_',today,'_',rndc)  ## array of sim ids
simheaders <- paste0(simheadbeg,sim_ids,simheadend) ## array of headers

## WRITING
simxml <- paste(simheaders,sim.params,outputs,'</Simulation>\n', sep="\n")

file.create(my_filename)
write(header, my_filename, append=FALSE)
sapply(simxml,FUN=w)
w(footer)

## RUNNING

outputdir <- paste0('headless_outputs/LHS_outs',today)
system(paste0('gama-headless.bat ',my_filename,' ',outputdir))

