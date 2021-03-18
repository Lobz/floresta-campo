
lines.par <- function(data,column.data, column.par,colors = make_colors(data[,column.par])) {
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
    fun(data,"n.broadleaf",...)
    title(paste0("Broadleaved population ",label))
    fun(data,"n.araucaria",...)
    title(paste0("Araucaria population ",label))
    fun(data,"circ.broadleaf",...)
    title(paste0("Broadleaf radius ",label))
    fun(data,"circ.araucaria",...)
    title(paste0("Araucaria radius ",label))
    par(mfrow=c(1,1))
}

plot.hypotheses <- function(data,fun,label="", ...) {
    par(mfrow=c(2,3))
    fun(data,"h1",...)
    title(paste0("H1: Patch has grown "))
    fun(data,"h2",...)
    title(paste0("H2: Araucaria pushed to edges "))
    fun(data,"h3a",...)
    title(paste0("H3a: nB Full > nB NoAr "))
    fun(data,"h3b",...)
    title(paste0("H3b: broadleafs can't expand without Ar"))
    fun(data,"h4a",...)
    title(paste0("H4a: nB Full < nB NoFi"))
    fun(data,"h4b",...)
    title(paste0("H4b: nA Full > nA NoFi "))
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
    props<-sapply(s,mean)
    labels<- round(cats[as.integer(names(props))],2)
    labels<- round(cats,2)
    barplot(rep(1,length(cats)),col="grey",space=0,border=F,names.arg=labels)
    barplot(props,col="darkblue",space=0,border=F,names.arg=labels,add=T)
}

plot.hyp <- function(data,column,column.par,...) {
    treatement<-data[,column.par]
    cats <- seq(min(treatement),max(treatement),length.out=10)
    t_cat <- factor(findInterval(treatement,cats), levels=1:10)
    finaltime <- max(data$time)
    finalvalues<- subset(data,time==finaltime)
    label <- paste0("after ",finaltime," years")
    plot.hypotheses(finalvalues,plot.boolean,label,par=column.par,t_cat=t_cat,cats=cats)
}

plot_all_hyps <- function(data,column.par) {
    treatement<-data[,column.par]
    cats <- seq(min(treatement),max(treatement),length.out=10)
    t_cat <- factor(findInterval(treatement,cats), levels=1:10)
    finaltime <- max(data$time)
    finalvalues<- subset(data,time==finaltime)
    label <- paste0("after ",finaltime," years")
    plot.hypotheses(finalvalues,plot.boolean,label,par=column.par,t_cat=t_cat,cats=cats)
}

plot_final_values <- function(data,column.par) {
    colors <- make_colors(data[,column.par])
    finaltime <- max(data$time)
    finalvalues<- subset(data,time==finaltime)
    label <- paste0("after ",finaltime," years")
    plot.fours.columns(finalvalues,plot.one.timestep,label,par=column.par,xlab=column.par,colors=colors)
}

