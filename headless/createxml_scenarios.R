

finalstep <- 2000
numreps <- 12
samplesize<-3

## filenaming
today <- paste0(strsplit(date()," ")[[1]][c(2:3,5)],collapse="")
gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
filename <- paste0("scenarios_",today,".xml")

file.create(filename)
## CONSTANTS
source("createxml.utils.R")

## VARIABLES
param.shared <- par.line('Shade tolerance ratio',2.0)

FMparams <- paste0(param.shared,'')
NAparams <- paste0(param.shared,'<Parameter name="initial_pop_ratio" type="INT" value="0" />')
NFparams <- paste0(param.shared,'<Parameter name="Wildfires" type="BOOLEAN" value="false" />')

my_names <- c("Full","NoAr","NoFi")
my_params <- c(FMparams,NAparams,NFparams)

## UNIQUE IDS
sim_ids <- paste0(my_names,'_',today,'_',rndc)  ## array of sim ids
simheaders <- paste0(simheadbeg,sim_ids,simheadend) ## array of headers

## WRITING
simxml <- paste(simheaders,my_params,outputs,'</Simulation>\n', sep="\n")

file.create(filename)
write(header, filename, append=FALSE)
sapply(simxml,FUN=w)
w(footer)

## RUNNING

outputdir <- 'headless_outputs/scenarios_outs'
system(paste0('gama-headless.bat ',filename,' ',outputdir))

