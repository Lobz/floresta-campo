
plot.lines <- function(data, column.data, color=1, ...) {
    ymax <- max(data[,column.data])
    xmax <- max(data$time)
    plot(NULL,NULL,ylim=c(0,ymax),xlim=c(0,xmax),xlab="Time",ylab=column.data, ...)
    lines.each(data,column.data,color)
}

plot.scenarios <- function(data, column.data, colors=c("purple", "orange", "darkgreen"), lwd=2, ...) {
    ymax <- max(data[,column.data])
    xmax <- max(data$time)
    plot(NULL,NULL,ylim=c(0,ymax),xlim=c(0,xmax),xlab="Time",ylab=column.data, ...)
    lines.each(subset(data,full),column.data,colors[1], lwd=lwd)
    lines.each(subset(data,noAr),column.data,colors[2], lwd=lwd)
    lines.each(subset(data,noFi),column.data,colors[3], lwd=lwd)
    legend(x=0, y=ymax, legend=c("Full", "NoAr", "NoFi"), col=colors, bg="white", lwd=c(lwd,lwd,lwd))
}

lines.par <- function(data, column.data, column.par,colors = make_colors(data[,column.par])) {
    ymax <- max(data[,column.data])
    xmax <- max(data$time)
    plot(NULL,NULL,ylim=c(0,ymax),xlim=c(0,xmax),xlab="Time",ylab=column.data)
    pars <- names(colors)
    for (p in pars) {
        dt <- subset(data,data[,column.par]==p)
        lines.each(dt,column.data,colors[p])
    }
}

lines.each <- function(data, column.data, color, alpha=0.2, ...) {
    if(length(unique(data$sim_unique_id)) == 1) {
        lines(data[,column.data] ~ data$time,col=color, ...)
    }
    else {
        color = adjustcolor(color, alpha=alpha)
        by (data,data$sim_unique_id, function (x) {
            lines(x[,column.data] ~ x$time,col=color, ...)
        })
    }
}

plot.fours.columns <- function(data, fun, label = "over time", ...) {
    par(mfrow=c(2,2))
    fun(data,"nB",...)
    title(paste0("Broadleaved population ",label))
    fun(data,"nA",...)
    title(paste0("Araucaria population ",label))
    fun(data,"rad95B",...)
    title(paste0("Broadleaf radius ",label))
    fun(data,"rad95A",...)
    title(paste0("Araucaria radius ",label))
    par(mfrow=c(1,1))
}

plot.hypotheses <- function(data,fun, ...) {
    par(mfrow=c(3,3))
    fun(data,"h1",...)
    title(paste0("H1: Patch has grown "))
    fun(data,"h2a",...)
    title(paste0("H2a: Araucaria grows at edges (Full Model)"))
    fun(data,"h2b",...)
    title(paste0("H2b: Araucaria grows at edges (No Fire)"))
    fun(data,"h3a",...)
    title(paste0("H3a: broadleafs expand faster with Araucaria "))
    fun(data,"h3b",...)
    title(paste0("H3b: broadleafs can't expand without Ar"))
    fun(data,"h4a",...)
    title(paste0("H4a: broadleafs expand faster without fire"))
    fun(data,"h4b",...)
    title(paste0("H4b: Araucaria population grows faster with fire "))
    fun(data,"h5",...)
    title(paste0("H5: Both groups expand, with Araucaria at edges"))
    par(mfrow=c(1,1))
}

make_colors <- function(data){
    data <- sort(data)
    params <- sort(unique(data))
    numpars <- length(params)
    colors <- colorRampPalette(c("darkblue","red"))(numpars)
    names(colors) <- params
    colors
}


plot.one.timestep <- function(d,y,par,colors,...) plot(d[,y]~d[,par],col=colors[as.character(d[,par])],...);

plot.boolean <- function(d,y,t_cat,cats,...) {
    values<-d[,y]
    s<-split(values,t_cat)
    props<-sapply(s,function(x) mean(x,na.rm=T))
    labels<- round(cats,2)
    barplot(rep(1,length(cats)),col="grey",space=0,border=F,names.arg=labels, ylab="frequency of hypothesis",...)
    barplot(props,col="darkblue",space=0,border=F,names.arg=labels,add=T,...)
}

plot.hyp <- function(data,column,column.par,...) {
    treatement<-data[,column.par]
    cats <- seq(min(treatement)-0.00001,max(treatement)+0.00001,length.out=10)
    t_cat <- factor(findInterval(treatement,cats), levels=1:10)
    finaltime <- max(data$time)
    finalvalues<- subset(data,time==finaltime)
    label <- paste0("after ",finaltime," years")
    plot.hypotheses(finalvalues,plot.boolean,label,par=column.par,t_cat=t_cat,cats=cats)
}

plot_all_hyps <- function(data,column.par) {
    treatement<-data[,column.par]
    cats <- seq(min(treatement),max(treatement),length.out=11)[1:10]
    t_cat <- factor(findInterval(treatement,cats), levels=1:10)
    plot.hypotheses(data,plot.boolean,par=column.par,t_cat=t_cat,cats=cats, xlab=column.par)
}

plot_final_values <- function(data,column.par) {
    colors <- make_colors(data[,column.par])
    label = "at simulation end"
    plot.fours.columns(data,plot.one.timestep,label,par=column.par,xlab=column.par,colors=colors)
}

pancake.plot <- function (results, x.par, y.par, col.par) {
    colors <- make_colors(results[,col.par])
    plot(results[,x.par], results[,y.par], col=colors[as.character(results[,col.par])], xlab=x.par, ylab=y.par)
}

## plot areas over time
arealengths <- function (filename, data, title="Lengths of dominance areas") {
    n <- nrow(data)

    barlengths <- matrix(c(data$circ05.araucaria, data$rad95B, data$rad95A), nrow=3, byrow=T)
    colnames(barlengths) <- data$time
    barlengths[3,] <- barlengths[3,] - barlengths[2,]
    barlengths[2,] <- barlengths[2,] - barlengths[1,]
    pdf(filename, width=7, height=5)
    par(lwd=3)
    barplot(height=barlengths, border=F, space=0, col=c("purple", "purple", "darkgreen"),
            angle=c(90,45,90), density=c(100,20,100),
            axes=T, ylab="average meters from center", xlab="years", main=title,
            legend.text=c("broadleaf dominance area (interior)", "coexistence area", "araucaria dominance area (edge)"),
            args.legend=list(x="topleft", border=F, bty = "n"))
    dev.off()
}
