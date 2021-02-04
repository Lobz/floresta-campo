
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

finalstep <- 1500
numreps <- 12

simtypes <- c("Full","NoAr","NoFi")
# mydirname <- paste0("fc",finalstep,'_',numreps,"_output")


# data <- read.csv(file=paste0(mydirname,"/data.csv"))
data <- read.csv(file=paste0("data.csv"))
# PLOTS
ymax <- max(data$n.broadleaf)
ymin <- 0
plot(n.broadleaf ~ time, data=data, subset=(scenario=="NoFi"),col=rgb(0,1,0,.05), xlim=c(0,finalstep),pch=20, ylim=c(ymin,ymax))
points(n.broadleaf ~ time, data=data, subset=(scenario=="NoAr"),col=rgb(1,0,0,.05),pch=20)
points(n.broadleaf ~ time, data=data, subset=(scenario=="Full"),col=rgb(0,0,1,.05),pch=20)
legend("topleft",simtypes, col=c("blue","red","green"),pch=20)
title("Broadleaved population over time")

ymax <- max(data$n.araucaria)
ymin <- 0
plot(n.araucaria ~ time, data=data, subset=(scenario=="Full"),col=rgb(0,0,1,.05), xlim=c(0,finalstep),pch=20, ylim=c(ymin,ymax))
points(n.araucaria ~ time, data=data, subset=(scenario=="NoFi"),col=rgb(0,1,0,.05),pch=20)
legend("topleft",c("Full","NoFi"), col=c("blue","green"),pch=20)
title("Araucaria population over time")

ymax <- max(data$circ.araucaria)
ymin <- 0
plot(circ.araucaria ~ time, data=data, subset=(scenario=="NoFi"),col=rgb(0,1,0,.05), xlim=c(0,finalstep),pch=20, ylim=c(ymin,ymax))
points(circ.araucaria ~ time, data=data, subset=(scenario=="Full"),col=rgb(0,0,1,.05),pch=20)
legend("topleft",c("Full","NoFi"), col=c("blue","green"),pch=20)
title("Araucaria occupancy radius over time")

ymax <- max(data$circ.broadleaf)
ymin <- 0
plot(circ.broadleaf ~ time, data=data, subset=(scenario=="NoFi"),col=rgb(0,1,0,.05), xlim=c(0,finalstep),pch=20, ylim=c(ymin,ymax))
points(circ.broadleaf ~ time, data=data, subset=(scenario=="NoAr"),col=rgb(1,0,0,.05),pch=20)
points(circ.broadleaf ~ time, data=data, subset=(scenario=="Full"),col=rgb(0,0,1,.05),pch=20)
legend("topleft",simtypes, col=c("blue","red","green"),pch=20)
title("Broadleaved occupancy radius over time")

ymax <- max(data$circ.araucaria - data$circ.broadleaf)
ymin <- 0
plot(circ.araucaria-circ.broadleaf ~ time, data=data, subset=(scenario=="Full"),col=rgb(0,0,1,.05), xlim=c(0,finalstep),pch=20, ylim=c(ymin,ymax))
title("Distance between araucaria edge and broadleafed edge")

# MODELS??
