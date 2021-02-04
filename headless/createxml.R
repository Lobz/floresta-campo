

finalstep <- 1500
numreps <- 12

gamlfile <- 'C:\\Users\\marin\\Documents\\Academico\\Gama\\floresta-campo\\FireandForest\\models\\instafire.gaml'
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
	  </Outputs>'

simheadbeg <- '
    <Simulation id="'
simheadend <- paste0('" sourcePath="',gamlfile,'" finalStep="',finalstep,'" experiment="fireandforest">')

footer <- '</Experiment_plan>'

## VARIABLES

FMparams <- '	  <Parameters>
        <!-- standard parameters -->
	  </Parameters>'


NAparams <- '	  <Parameters>
	    <Parameter name="Initial Araucaria pop" type="INT" value="0" />
	  </Parameters>'

      
NFparams <- '	  <Parameters>
	    <Parameter name="Wildfires" type="BOOLEAN" value="false" />
	  </Parameters>'

my_names <- c("Full","NoAr","NoFi")
my_params <- c(FMparams,NAparams,NFparams)

## WRITING
      
write(header, filename, append=FALSE)

for(i in 1:numreps) {
    for(type in 1:3) {
        ## make sim header
        sim_id <- paste0(my_names[type],i)
        simheader <- paste0(simheadbeg,sim_id,simheadend)
        w(simheader)
        w(my_params[type])
        w(outputs)
        w('	</Simulation>')
    }
}

w(footer)
