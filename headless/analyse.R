
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

source("plotsfuns.utils.R")
### PLOTS
datafull <- read.csv(file=myfilename,stringsAsFactors=T)
datafull$initial_pop_ratio <- 1.0*datafull$initial.pop.araucaria/datafull$initial.pop.broadleaf

data <- subset(datafull, shade.threshold.ratio > 0)

plot_final_values(data,"wildfire_rate")

## get one parameter set
parcols <- 6:13 ## cols with parameter values
NoArdata <- subset(data,initial_pop_araucaria==0)
NoFidata <- subset(data,wildfire_rate==0)
Fulldata <- subset(data,initial_pop_araucaria>0 & wildfire_rate >0)

NoFi.one <- subset(NoFidata,sim_unique_id==NoFidata$sim_unique_id[1])
NoAr.one <- subset(NoArdata, shade_threshold_ratio == NoFi.one$shade_threshold_ratio[1])
Full.one <- subset(Fulldata, shade_threshold_ratio == NoFi.one$shade_threshold_ratio[1])

## extract variables
finaltime <- max(data$time)

nA.NoFi.final <- subset(NoFi.one,time==finaltime)$n.araucaria
nA.NoFi.initial <- subset(NoFi.one,time==1)$n.araucaria
nA.NoAr.final <- subset(NoAr.one,time==finaltime)$n.araucaria
nA.NoAr.initial <- subset(NoAr.one,time==1)$n.araucaria
nA.Full.final <- subset(Full.one,time==finaltime)$n.araucaria
nA.Full.initial <- subset(Full.one,time==1)$n.araucaria

nB.NoFi.final <- subset(NoFi.one,time==finaltime)$n.broadleaf
nB.NoFi.initial <- subset(NoFi.one,time==1)$n.broadleaf
nB.NoAr.final <- subset(NoAr.one,time==finaltime)$n.broadleaf
nB.NoAr.initial <- subset(NoAr.one,time==1)$n.broadleaf
nB.Full.final <- subset(Full.one,time==finaltime)$n.broadleaf
nB.Full.initial <- subset(Full.one,time==1)$n.broadleaf

radA.NoFi.final <- subset(NoFi.one,time==finaltime)$circ.araucaria
radA.NoFi.initial <- subset(NoFi.one,time==1)$circ.araucaria
radA.NoAr.final <- subset(NoAr.one,time==finaltime)$circ.araucaria
radA.NoAr.initial <- subset(NoAr.one,time==1)$circ.araucaria
radA.Full.final <- subset(Full.one,time==finaltime)$circ.araucaria
radA.Full.initial <- subset(Full.one,time==1)$circ.araucaria
rad
radB.NoFi.final <- subset(NoFi.one,time==finaltime)$circ.broadleaf
radB.NoFi.initial <- subset(NoFi.one,time==1)$circ.broadleaf
radB.NoAr.final <- subset(NoAr.one,time==finaltime)$circ.broadleaf
radB.NoAr.initial <- subset(NoAr.one,time==1)$circ.broadleaf
radB.Full.final <- subset(Full.one,time==finaltime)$circ.broadleaf
radB.Full.initial <- subset(Full.one,time==1)$circ.broadleaf


## array of hypotheses



### full plots
plot.fours.columns(data,function(d,x) lines.par(d,x,"shade.threshold.ratio",colors))