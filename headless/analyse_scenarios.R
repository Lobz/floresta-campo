
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

finalstep <- 1500
numreps <- 20

simtypes <- c("Full","NoAr","NoFi")
# mydirname <- paste0("fc",finalstep,'_',numreps,"_output")


# data <- read.csv(file=paste0(mydirname,"/data.csv"))
data <- read.csv(file=my_filename,stringsAsFactors=T)
# AGGREGATE

agg.data <- aggregate(data[,1:4], by=list(time=data$time, scenario=data$scenario), FUN=median)
qua.sup <- aggregate(data[,1:4], by=list(time=data$time, scenario=data$scenario), FUN=function(x) quantile(x,.90,na.rm=T))
qua.inf <- aggregate(data[,1:4], by=list(time=data$time, scenario=data$scenario), FUN=function(x) quantile(x,.10,na.rm=T))

### PLOTS
colors <- c(NoFi=rgb(0,1,0,0.5), Full=rgb(0,0,1,0.5), NoAr=rgb(1,0,0,0.5))
plot.col <- function(column, plot.fun) {
    ymax <- max(data[,column])
    ymin <- 1
    plot(NULL,NULL,xlim=c(0,finalstep),ylim=c(ymin,ymax),ylab=column,xlab="time")
    legend("topleft",simtypes, col=colors[simtypes],lty=1)
    for(t in simtypes) {
        plot.fun(column,t)
    }
}

plot.means <- function(column,t) {
    c <- colors[t]
    time <- agg.data[agg.data$scenario==t,"time"]
    mean <- agg.data[agg.data$scenario==t,column]
    sup <- qua.sup[qua.sup$scenario==t,column]
    inf <- qua.inf[qua.inf$scenario==t,column]

    lines(mean~time,col=c,lwd=2)
    lines(sup~time,col=c)
    lines(inf~time,col=c)
}

plot.full <- function(column,t) {
    c <- colors[t]
    subs <- subset(data,scenario==t)

    for (i in 1:numreps) {
        time <- subs[subs$rep_num==i,"time"]
        values <- subs[subs$rep_num==i,column]
        lines(values~time, col=colors[t])
    }
}


plot.compare.scenarios <- function(plot.fun) {
    par(mfrow=c(2,2))
    plot.col("n.broadleaf",plot.fun)
    title("Broadleaved population over time")
    plot.col("n.araucaria",plot.fun)
    title("Araucaria population over time")
    plot.col("circ.broadleaf",plot.fun)
    title("Broadleaf radius over time")
    plot.col("circ.araucaria",plot.fun)
    title("Araucaria radius over time")
    par(mfrow=c(1,1))
}

plot.compare.scenarios(plot.means)
plot.compare.scenarios(plot.full)

logisticgrowth <- function(r,K,N0,t){ (1.0*K/((K-N0)*exp(-r*t)/N0 +1)) }

INITIALPOP<-100
fit.data.log <- function(pop,times) {nls(pop~logisticgrowth(r,K,N0,times),start=list(r=1,K=max(pop),N0=INITIALPOP))}
reglog <- function(dt){tryCatch(coef(fit.data.log(dt)),error=function(e){ c(NA,NA) })}

logito <- function(x) { log(x) - log(K-x) }

plot.col("n.broadleaf",plot.means)

one.run <- subset(data,scenario=="NoAr" & rep_num=="1")
fittedrun<- fit.data.log(one.run$n.broadleaf, one.run$time)
    c <- coef(fittedrun)
    r <- c["r"]
    K <- c["K"]
    N0 <- c["N0"]
lg <- function(t) {logisticgrowth(r,K,N0,t)}


    plot.col("n.broadleaf",plot.full)
    title("Broadleaved population over time")
    curve(lg(x),add=T,lwd=3)

fit.lm<- lm(log(one.run$n.broadleaf) ~ one.run$time)
    c <- coef(fit.lm)
    a <- c[1]
    b <- c[2]

curve(exp(a+b*x),add=T)


