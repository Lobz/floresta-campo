
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

source("plotsfuns.utils.R")
### PLOTS
datafull <- read.csv(file=myfilename,stringsAsFactors=T)
datafull$initial_pop_ratio <- 1.0*datafull$initial.pop.araucaria/datafull$initial.pop.broadleaf

data <- subset(datafull, shade.threshold.ratio > 0)

plot_final_values(data,"wildfire_rate")



### full plots
plot.fours.columns(data,function(d,x) lines.par(d,x,"shade.threshold.ratio",colors))

avmaxvalues <- aggregate(maxvalues,by=list(shade.threshold.ratio=maxvalues$shade.threshold.ratio),FUN=mean)
avfinalvalues <- aggregate(finalvalues,by=list(shade.threshold.ratio=finalvalues$shade.threshold.ratio),FUN=mean)
my_LHS_pars$data <- avmaxvalues[,par.names]

my_LHS <- tell(my_LHS_pars, avmaxvalues[,my_outputnames])
