
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

source("plotsfuns.utils.R")
### PLOTS
datafull <- read.csv(file=myfilename,stringsAsFactors=T)

data <- datafull

plot_final_values(data,"wildfire_rate")

test.hypotheses <- function(data) {
    ## get one parameter set
    NoAr.one <- subset(data,initial_pop_araucaria==0)
    NoFi.one <- subset(data,wildfire_rate==0)
    Full.one <- subset(data,initial_pop_araucaria>0 & wildfire_rate >0)

    ## extract variables
    finaltime <- max(data$time)

    NoFi.final <- subset(NoFi.one,time==finaltime)
    NoFi.initial <- subset(NoFi.one,time==1)
    NoAr.final <- subset(NoAr.one,time==finaltime)
    NoAr.initial <- subset(NoAr.one,time==1)
    Full.final <- subset(Full.one,time==finaltime)
    Full.initial <- subset(Full.one,time==1)

    ## hyp1 : patch will grow
    hyp1 <- max(Full.final$circ.araucaria,Full.final$circ.broadleaf) > max(Full.initial$circ.araucaria,Full.initial$circ.broadleaf)
    ## hyp2 : araucaria is on the edge
    hyp2 <- Full.final$circ.araucaria > Full.final$circ.broadleaf
    ## hyp3 : broadleaf grows better with araucaria / can't grow without araucaria
    hyp3a <- Full.final$n.broadleaf > NoAr.final$n.broadleaf
    hyp3b <- NoAr.final$circ.broadleaf <= Full.final$circ.broadleaf
    ## hyp4 : without fire, broadleaf grows and competes with araucaria
    hyp4a <- Full.final$n.broadleaf < NoFi.final$n.broadleaf
    hyp4b <- Full.final$n.araucaria > NoFi.final$n.araucaria
    ## hyp5 : broadleaf expansion tracks araucaria expansion
    hyp5 <- NA
    ## extinction full check
    ext <- max(Full.final$circ.araucaria,Full.final$circ.broadleaf) ==0

    ## error catching
    if(nrow(Full.one)==0) {
        hyp1=hyp2=hyp3a=hyp3b=hyp4a=hyp4b=hyp5=extinction=NA;
    }
    if(nrow(NoFi.one)==0) {
        hyp4a=NA;
        hyp4b=NA;
    }
    if(nrow(NoAr.one)==0) {
        hyp3a=hyp3b=NA;
    }

    ## array of hypotheses
    hypotheses <- c(hyp1,hyp2,hyp3a,hyp3b,hyp4a,hyp4b,hyp5,ext)
    names(hypotheses) <- c("h1","h2","h3a","h3b","h4a","h4b","h5","full_extinction")
    hypotheses
}

## turn indices into factors
data$par_group <- as.factor(data$par_group)

results.raw <- by(data,data$par_group,test.hypotheses)
results <- as.data.frame(do.call(rbind,results.raw))
results$par_group<-as.numeric(rownames(results))
### merge back into values of data
finaltime <- max(data$time)
finalvalues<- subset(data,time==finaltime)
results_finalvalues <- merge(results,finalvalues,by="par_group")
results_par <- aggregate(results_finalvalues,by=list(par_group=results_finalvalues$par_group),function(x) x[1])

plot_final_values(finalvalues,"araucaria_base_flammablity")
plot_all_hyps(results_par,"araucaria_base_flammability")
plot.hypotheses(results[!results$full,],function(x,c,...) barplot(table(x[,c]),...))
### full plots
plot.fours.columns(data,function(d,x) lines.par(d,x,"shade.threshold.ratio",colors))