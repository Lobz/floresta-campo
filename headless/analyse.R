
# make sure you've used createxml to create the xml and xmlToCsv to interpret the results

finalstep <- 1000
numreps <- 10

simtypes <- c("Full","NoAr","NoFi")
dirname <- paste0("fc",finalstep,'_',numreps,"_output")


data <- read.csv(file=paste0(dirname,"/data.csv"))

plot(n.broadleaf ~ time, data=data, subset=(scenario=="NoFi"),col=rgb(0,0,1,.1))
points(n.broadleaf ~ time, data=data, subset=(scenario=="NoAr"),col=rgb(1,0,0,.1))
points(n.broadleaf ~ time, data=data, subset=(scenario=="Full"),col=rgb(0,1,0,.1))

plot(n.araucaria ~ time, data=data, subset=(scenario=="NoFi"),col=rgb(0,0,1,.1))
points(n.araucaria ~ time, data=data, subset=(scenario=="NoAr"),col=rgb(1,0,0,.1))
points(n.araucaria ~ time, data=data, subset=(scenario=="Full"),col=rgb(0,1,0,.1))

plot(circ.araucaria ~ time, data=data, subset=(scenario=="NoFi"),col=rgb(0,0,1,.1))
points(circ.araucaria ~ time, data=data, subset=(scenario=="NoAr"),col=rgb(1,0,0,.1))
points(circ.araucaria ~ time, data=data, subset=(scenario=="Full"),col=rgb(0,1,0,.1))

plot(circ.broadleaf ~ time, data=data, subset=(scenario=="NoFi"),col=rgb(0,0,1,.1))
points(circ.broadleaf ~ time, data=data, subset=(scenario=="NoAr"),col=rgb(1,0,0,.1))
points(circ.broadleaf ~ time, data=data, subset=(scenario=="Full"),col=rgb(0,1,0,.1))


