
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

source("plotsfuns.utils.R")
### PLOTS
datafull <- read.csv(file=myfilename,stringsAsFactors=T)

data <- datafull
data$edge_range <- data$circ.araucaria - data$circ.broadleaf
data$noAr <- data$initial_pop_araucaria ==0
data$noFi <- data$wildfire_rate ==0
data$full <- !data$noAr & !data$noFi

plot_final_values(data,"wildfire_rate")

test.hypotheses <- function(data) {
    ## separate scenarios
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
    hyp3b <- NoAr.final$circ.broadleaf <= NoAr.initial$circ.broadleaf
    ## hyp4 : without fire, broadleaf grows and competes with araucaria
    hyp4a <- Full.final$n.broadleaf < NoFi.final$n.broadleaf
    hyp4b <- Full.final$n.araucaria > NoFi.final$n.araucaria
    ## hyp5 : broadleaf expansion tracks araucaria expansion
    hyp5 <- NA
#   Full.half <- subset(Full.one, time>500) ## eliminate beggining
#   maxpop <- max(Full.half$n.araucaria)
#   maxpoptime <- subset(Full.half,n.araucaria==maxpop)$time
#   Full.half <- subset(Full.one, time < maxpoptime)
#   anova(lm(edge_range~time, Full.half))
    

    ## extinction full check
    ext <- Full.final$n.araucaria+Full.final$n.broadleaf < 10

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

test.hypotheses.all <- function(data) {
    ## turn indices into factors
    data$par_group <- as.factor(data$par_group)

    results.raw <- by(data,data$par_group,test.hypotheses)
    results <- as.data.frame(do.call(rbind,results.raw))
    results
}
results<- test.hypotheses.all(data)
results$par_group<-as.numeric(rownames(results))
### merge back into values of data
finaltime <- max(data$time)
finalvalues<- subset(data,time==finaltime)
results_finalvalues <- merge(results,finalvalues,by="par_group")
results_par <- aggregate(results_finalvalues,by=list(par_group=results_finalvalues$par_group),function(x) {
    tryCatch(max(x),error=function(e)NA)
})
data_hyps<- merge(data,results,by="par_group")

plot_final_values(subset(finalvalues,full),"araucaria_base_flammability")
plot_all_hyps(results_par,"grass_flammability")
plot.hypotheses(results[!results$full_extinction,],function(x,c,...) barplot(table(x[,c]),...))
plot.hyp(results_par,"full_extinction","wildfire_rate")
### full plots
plot.fours.columns(data,lines.par,column.par="wildfire_rate")
weirds <- (subset(data_hyps,h1&!(h2&h3a&h3b&h4a&h4b)))
weirds <- weirds[order(weirds$time),]
plot.fours.columns(weirds,lines.par,column.par="wildfire_rate")
weirds.half <- subset(weirds, time<1000)
res.weirds<- test.hypotheses.all(weirds.half)

lines.par(subset(data,wildfire_rate > 0 & initial_pop_araucaria > 0 & time>500),"edge_range","araucaria_base_flammability")

# crowding
tree_canopy_area <- pi*5*5
tile_area<-10*10
shade_percent <- tree_canopy_area/tile_area

expected_crowd <- function(shade_threshold) {
    ceiling(shade_threshold/shade_percent)
}

