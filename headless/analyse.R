
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

finalstep <- 2000
numreps <- 100

myfilename <- paste0("fc",finalstep,'_',numreps,"_outputdata_fire.csv")

### PLOTS
datafull <- read.csv(file=paste0("data/",myfilename),stringsAsFactors=T)
data<-subset(datafull,wildfire.rate<0.1)
params <- sort(unique(data$wildfire.rate))
numpars <- length(params)
colors <- colorRampPalette(c("darkblue","red"),bias=0.001)(numpars)
names(colors) <- params
plot.fours.columns()

lines.par <- function(data,column.data, column.par,colors) {
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

plot.fours.columns <- function(plot.fun) {
    par(mfrow=c(2,2))
    plot.col("n.broadleaf")
    title("Broadleaved population over time")
    plot.col("n.araucaria")
    title("Araucaria population over time")
    plot.col("circ.broadleaf")
    title("Broadleaf radius over time")
    plot.col("circ.araucaria")
    title("Araucaria radius over time")
    par(mfrow=c(1,1))
}


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


