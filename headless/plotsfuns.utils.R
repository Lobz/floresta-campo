
lines.par <- function(data,column.data, column.par,colors = make_colors(data, column.par)) {
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

plot.fours.columns <- function(data, fun, label = "over time", ...) {
    par(mfrow=c(2,2))
    fun(data,"n.broadleaf",ylab="Population",...)
    title(paste0("Broadleaved population ",label))
    fun(data,"n.araucaria",ylab="Population",...)
    title(paste0("Araucaria population ",label))
    fun(data,"circ.broadleaf",ylab="Radius",...)
    title(paste0("Broadleaf radius ",label))
    fun(data,"circ.araucaria",ylab="Radius",...)
    title(paste0("Araucaria radius ",label))
    par(mfrow=c(1,1))
}

make_colors <- function(data,column.par){
    data <- data[order(data[,column.par]),]
    params <- sort(unique(data[,column.par]))
    numpars <- length(params)
    colors <- colorRampPalette(c("darkblue","red"))(numpars)
    names(colors) <- params
    colors
}


plot.one.timestep <- function(d,y,par,colors,...) plot(d[,y]~d[,par],col=colors[as.character(d[,par])],...);


plot_final_values <- function(data,column.par) {
    colors <- make_colors(data,column.par)
    finaltime <- max(data$time)
    finalvalues<- subset(data,time==finaltime)
    label <- paste0("after ",finaltime," years")
    plot.fours.columns(finalvalues,plot.one.timestep,label,par=column.par,xlab=column.par,colors=colors)
}
