

finalstep <- 2000
numreps <- 1
samplesize<-3

## filenaming
today <- paste0(strsplit(date()," ")[[1]][c(2:3,5)],collapse="")
gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
my_filename <- paste0("scenarios_",today,".xml")

## CONSTANTS
source("createxml.utils.R")

## VARIABLES
param.shared <- ""

FMparams <- paste0(param.shared,'')
NAparams <- paste0(param.shared,'<Parameter name="initial_pop_ratio" type="INT" value="0" />')
NFparams <- paste0(param.shared,'<Parameter name="Wildfires" type="BOOLEAN" value="false" />')

my_names <- c("Full","NoAr","NoFi")
my_params <- c(FMparams,NAparams,NFparams)

## UNIQUE IDS
n <- length(my_params)
rndc <- do.call(paste0, replicate(5, sample(chars, n , TRUE), FALSE)) # generate unique ids
sim_ids <- paste0(my_names,'_',today,'_',rndc)  ## array of sim ids
simheaders <- paste0(simheadbeg,sim_ids,simheadend) ## array of headers

## WRITING
simxml <- paste(simheaders,my_params,outputs,'</Simulation>\n', sep="\n")

file.create(my_filename)
write(header, my_filename, append=FALSE)
sapply(simxml,FUN=w)
w(footer)
## RUNNING

outputdir <- 'headless_outputs/scenarios_outs'
system(paste0('gama-headless.bat ',my_filename,' ',outputdir))

