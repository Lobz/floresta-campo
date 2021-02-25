
## filenaming
today <- paste0(strsplit(date()," ")[[1]][c(2:3,5)],collapse="")
gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
filename <- paste0("LHS_",today,".xml")

## parametrizing
finalstep <- 700
samplesize <- 40
numreps <- 3

par.names <- c("Chance of fire","shade_threshold_araucaria","Shade tolerance ratio","initial_pop_ratio","araucaria_base_flammability")
q.arg <- list(list(min=0.5,max=2.0),list(min=0.5,max=1.5),list(min=1.0,max=3.0),list(min=0.0,max=1.0),list(min=0.5,max=1.0))

## creating parameter data.frame
library(pse)
my_LHS_pars <- LHS(model=NULL,
                  factors=par.names,
                  N=samplesize,
                  q.arg=q.arg,
                  repetitions=numreps)

par.data <- my_LHS_pars$data

## CONSTANTS & UTILS
source("createxml.utils.R")

### Parameters
sim.params<-apply(par.data,1,par.row)

## UNIQUE IDS
sim_ids <- paste0('LHS_',today,'_',rndc)  ## array of sim ids
simheaders <- paste0(simheadbeg,sim_ids,simheadend) ## array of headers

## WRITING
simxml <- paste(simheaders,sim.params,outputs,'</Simulation>\n', sep="\n")

file.create(filename)
write(header, filename, append=FALSE)
sapply(simxml,FUN=w)
w(footer)

outputdir <- 'flam_outs'
system(paste0('gama-headless.bat ',filename,' ',outputdir))

