

finalstep <- 20
numreps <- 3


fire <- rep(seq(0.05,0.5,by=0.01),numreps)
shade.threshold.araucaria <- rep(seq(0.3,1.8,by=0.05),numreps)
shade.threshold.ratio <- rep(seq(1.0,3.0,by=0.01),numreps)
initial.pop.araucaria <- rep(seq(0,100,by=1),numreps)
araucaria_base_flammability <- rep(seq(0.5,1,by=0.01),numreps)

today <- paste0(strsplit(date()," ")[[1]][c(2:3,5)],collapse="")
gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
filename <- paste0("flam0.5to1by001_",today,".xml")

## CONSTANTS

header <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<Experiment_plan>
"
outputs <- readChar("outputs.xmlpart", file.info("outputs.xmlpart")$size)

simheadbeg <- '
    <Simulation id="'
simheadend <- paste0('" sourcePath="',gamlfile,'" finalStep="',finalstep,'" experiment="fireandforest">')

footer <- '</Experiment_plan>'

### Parameters

params <- function(name,p) {
  paste0('<Parameter name="',name,'" type="FLOAT" value="',p,'" />')
}
#simparams <- params("shade_threshold_araucaria",shade_threshold_araucaria) # array of parameters
simparams <- params("araucaria_base_flammability",araucaria_base_flammability) # array of parameters

### UNIQUE IDS

parameters <- araucaria_base_flammability
n <- length(params)
chars <- c(LETTERS,letters,0:9)
rndc <- do.call(paste0, replicate(5, sample(chars, n , TRUE), FALSE)) # generate unique ids

sim_ids <- paste0('FLAM_',parameters,'_',rndc)  ## array of sim ids
simheaders <- paste0(simheadbeg,sim_ids,simheadend) ## array of headers

## WRITING

simxml <- paste(simheaders,simparams,outputs,'</Simulation>\n', sep="\n")

w <- function (str) {
    write(str,file=filename,append=T)
}

file.create(filename)
write(header, filename, append=FALSE)
sapply(simxml,FUN=w)
w(footer)

outputdir <- 'flam_outs'
system('set-gamadir.bat')
system(paste0('gama-headless.bat ',filename,' ',outputdir))

