

finalstep <- 2000
numreps <- 100

gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
filename <- paste0("fc",finalstep,'_',numreps,".xml")


## CONSTANTS

header <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<Experiment_plan>
"
outputs <- '
	  <Outputs>
	    <Output id="1" name="Number of araucaria trees" framerate="1" />
	    <Output id="2" name="Number of broadleaved trees" framerate="1" />
	    <Output id="3" name="Circle size for araucaria trees" framerate="1" />
	    <Output id="4" name="Circle size for broadleaved trees" framerate="1" />
	    <Output id="5" name="Size of fire" framerate="1" />
      	<Output id="6" name="Chance of fire" framerate="1" />
      	<Output id="7" name="Initial Araucaria pop" framerate="1" />
     	<Output id="8" name="Initial broadleaved pop" framerate="1" />
      	<Output id="9" name="Araucaria shade tolerance" framerate="1" />
      	<Output id="10" name="Shade tolerance ratio" framerate="1" />
	</Outputs>'

simheadbeg <- '
    <Simulation id="'
simheadend <- paste0('" sourcePath="',gamlfile,'" finalStep="',finalstep,'" experiment="fireandforest">')

footer <- '</Experiment_plan>'

### Parameters

fire <- seq(0.05,0.5,length.out=numreps)
shade_threshold_araucaria <- seq(0.3,1.8,length.out=numreps)
shade_threshold_ratio <- seq(1.0,2.5,length.out=numreps)
params <- function(name,p) {
  paste0('<Parameter name="',name,'" type="FLOAT" value="',p,'" />')
}
#simparams <- params("shade_threshold_araucaria",shade_threshold_araucaria) # array of parameters
simparams <- params("shade_threshold_ratio",shade_threshold_ratio) # array of parameters

### UNIQUE IDS

n <- length(parameters)
chars <- c(LETTERS,letters,0:9)
rndc <- do.call(paste0, replicate(5, sample(chars, n , TRUE), FALSE)) # generate unique ids

sim_ids <- paste0('RATIO_',parameters,'_',rndc)  ## array of sim ids
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

system('set GAMADIR=C:\\Users\\marin\\Documents\\GAMA')
outputdir <- 'test_outs'
system(paste0('gama-headless.bat ',filename,' ',outputdir))
