
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

finalstep <- 2000
numreps <- 100

### PLOTS
datafull <- read.csv(file=paste0(myfilename),stringsAsFactors=T)
data<-subset(datafull,shade.threshold.ratio >0)
params <- sort(unique(data$shade.threshold.ratio))
numpars <- length(params)
colors <- colorRampPalette(c("darkblue","red"),bias=0.001)(numpars)
names(colors) <- params
plot.fours.columns(data,"shade.threshold.ratio",colors)

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

plot.fours.columns <- function(data, column.par, colors) {
    par(mfrow=c(2,2))
    lines.par(data,"n.broadleaf",column.par,colors)
    title("Broadleaved population over time")
    lines.par(data,"n.araucaria",column.par,colors)
    title("Araucaria population over time")
    lines.par(data,"circ.broadleaf",column.par,colors)
    title("Broadleaf radius over time")
    lines.par(data,"circ.araucaria",column.par,colors)
    title("Araucaria radius over time")
    par(mfrow=c(1,1))
}
