
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

finalstep <- 2000
numreps <- 3

### PLOTS
datafull <- read.csv(file=myfilename,stringsAsFactors=T)

data <- subset(datafull, shade.threshold.ratio > 0)
data <- data[order(data$shade.threshold.ratio),]
params <- sort(unique(data$shade.threshold.ratio))
numpars <- length(params)
colors <- colorRampPalette(c("darkblue","red"))(numpars)
names(colors) <- params
plot.fours.columns(data,function(d,x) lines.par(d,x,"shade.threshold.ratio",colors))

plot.one.timestep <- function(d,x) plot(d[,x]~d$shade.threshold.ratio,col=colors[as.character(params)]);
finalvalues<- subset(data,time==max(data$time))
plot.fours.columns(finalvalues,plot.one.timestep,"at end of simulation")
maxvalues<- aggregate(data[,-ncol(data)],by=list(sim_unique_id=data$sim_unique_id),FUN=max)
plot.fours.columns(maxvalues,plot.one.timestep, "(maximum)")

lines.par <- function(data,column.data, column.par,colors) {
    ymax <- max(data[,column.data])
    xmax <- max(data$time)
    plot(NULL,NULL,ylim=c(0,ymax),xlim=c(0,xmax),xlab="Time",ylab=column.data)
    pars <- names(colors)
    for (p in pars) {
        dt <- subset(data,data[,column.par]==p)
        lines.each(dt,column.data,colors[p])
    }
}

lines.each <- function(data,column.data,color) {
    if(length(unique(data$sim_unique_id)) == 1) {
        lines(data[,column.data] ~ data$time,col=color)
    }
    else {
        by (data,data$sim_unique_id, function (x) {
            lines(x[,column.data] ~ x$time,col=color)
        })
    }
}

plot.fours.columns <- function(data, fun, label = "over time") {
    par(mfrow=c(2,2))
    fun(data,"n.broadleaf")
    title(paste0("Broadleaved population ",label))
    fun(data,"n.araucaria")
    title(paste0("Araucaria population ",label))
    fun(data,"circ.broadleaf")
    title(paste0("Broadleaf radius ",label))
    fun(data,"circ.araucaria")
    title(paste0("Araucaria radius ",label))
    par(mfrow=c(1,1))
}
