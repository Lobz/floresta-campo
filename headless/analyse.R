
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

source("plotsfuns.utils.R")
### PLOTS
datafull <- read.csv(file=myfilename,stringsAsFactors=T)
datafull$initial_pop_ratio <- 1.0*datafull$initial.pop.araucaria/datafull$initial.pop.broadleaf

data <- subset(datafull, shade.threshold.ratio > 0)
data <- data[order(data$shade.threshold.ratio),]
params <- sort(unique(data$shade.threshold.ratio))
numpars <- length(params)
colors <- colorRampPalette(c("darkblue","red"))(numpars)
names(colors) <- params
plot.fours.columns(data,function(d,x) lines.par(d,x,"shade.threshold.ratio",colors))

plot.one.timestep <- function(d,x) plot(d[,x]~d$shade.threshold.ratio,col=colors[as.character(d[,"shade.threshold.ratio"])]);
finalvalues<- subset(data,time==max(data$time))
plot.fours.columns(finalvalues,plot.one.timestep,"at end of simulation")
maxvalues<- aggregate(data[,-(ncol(data)-1)],by=list(sim_unique_id=data$sim_unique_id),FUN=max)
plot.fours.columns(maxvalues,plot.one.timestep, "(maximum)")

avmaxvalues <- aggregate(maxvalues,by=list(shade.threshold.ratio=maxvalues$shade.threshold.ratio),FUN=mean)
avfinalvalues <- aggregate(finalvalues,by=list(shade.threshold.ratio=finalvalues$shade.threshold.ratio),FUN=mean)
my_LHS_pars$data <- avmaxvalues[,par.names]

my_LHS <- tell(my_LHS_pars, avmaxvalues[,my_outputnames])
