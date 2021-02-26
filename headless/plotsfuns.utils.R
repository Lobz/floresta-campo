
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
