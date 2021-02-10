

finalstep <- 15
numreps <- 11

gamlfile <- '..\\FireandForest\\models\\instafire.gaml'
filename <- paste0("fc",finalstep,'_',numreps,".xml")

file.create(filename)
w <- function (str) {
    write(str,file=filename,append=T)
}

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
	  </Outputs>'

simheadbeg <- '
    <Simulation id="'
simheadend <- paste0('" sourcePath="',gamlfile,'" finalStep="',finalstep,'" experiment="fireandforest">')

footer <- '</Experiment_plan>'

### Parameters

parameters <- seq(0,1,length.out=numreps)

write_params <- function(p) {
  w(paste0('<Parameter name="Chance of fire" type="FLOAT" value="',p,'" />'))
}



## WRITING
      
write(header, filename, append=FALSE)

for(p in parameters) {
        ## make sim header
        sim_id <- paste0('FIRE_',p,'_')
        simheader <- paste0(simheadbeg,sim_id,simheadend)
        w(simheader)
        write_params(p)
        w(outputs)
        w('	</Simulation>')
}

w(footer)
