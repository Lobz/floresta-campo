
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
    NoAr.one <- subset(data,noAr)
    NoFi.one <- subset(data,noFi)
    Full.one <- subset(data,full)

    ## extract variables

    maxtime <- max(Full.one$time)
    Full.final <- subset(Full.one,time==maxtime)

    ## extinction full check
    ext <- Full.final$n.araucaria+Full.final$n.broadleaf < 10

    ## loglikelyhood functions for growthrate comparisons
    compare.two <- function (x1, t1, x2, t2) {
        LL.same <- function(a1, a2, b, sd){
        -sum(dnorm(x1, mean= a1 + b*t1, sd=sd, log=T)) -sum(dnorm(x2, mean= a2 + b*t2, sd=sd, log=T))
        }

        LL.diff <- function(a1, a2, b1, b2, sd){
        -sum(dnorm(x1, mean= a1 + b1*t1, sd=sd, log=T)) -sum(dnorm(x2, mean= a2 + b2*t2, sd=sd, log=T))
        }

        lm1 <- lm(x1 ~ t1)
        lm2 <- lm(x2 ~ t2)

        sa1 <- coef(lm1)[1]
        sa2 <- coef(lm2)[1]
        sb1 <- coef(lm1)[2]
        sb2 <- coef(lm2)[2]

        model.same <- mle2(LL.same, start=list(a1=sa1, a2=sa2, b = mean(sb1,sb2), sd=1))
        model.diff <- mle2(LL.diff, start=list(a1=sa1, a2=sa2, b1=sb1, b2=sb2, sd=1))

        aic <- AIC(model.same,model.diff)$AIC
        if (aic[1] < aic[2] - 2) {
            return (FALSE);
        }
        if (aic[1] > aic[2] + 2) {
            ## difference
            if (sb1 > sb2) {
                return (1);
            }
            else {
                return (2);
            }
        }
        return (FALSE);
    }

    ## hyp1 : patch will grow
    model <- lm(circ.broadleaf ~ time, Full.one)
    hyp1 <- !ext && coef(model)[2] > 0 && summary(model)$coefficients[2,4] < 0.05
    ## hyp2 : araucaria is on the edge
    hyp2 <- median(Full.one$edge_range) > 10
    ## hyp3 : broadleaf grows better with araucaria / can't grow without araucaria
    model_NoAr <- lm(circ.broadleaf ~ time, NoAr.one)
    growth_NoAr <- coef(model_NoAr)[2] > 0 && summary(model_NoAr)$coefficients[2,4] < 0.05
    aic <- compare.two(Full.one$circ.broadleaf, Full.one$time, NoAr.one$circ.broadleaf, NoAr.one$time)
    hyp3a <- (aic == 1)
    hyp3b <- !growth_NoAr
    ## hyp4 : without fire, broadleaf grows and competes with araucaria
    aic <- compare.two(Full.one$circ.broadleaf, Full.one$time, Full.one$circ.broadleaf, Full.one$time)
    hyp4a <- (aic == 2)
    aic <- compare.two(Full.one$n.araucaria, Full.one$time, NoFi.one$n.araucaria, NoFi.one$time)
    hyp4b <- (aic == 1)
    ## hyp5 : broadleaf expansion tracks araucaria expansion
    hyp5 <- FALSE

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

# crowding
tree_canopy_area <- pi*5*5
tile_area<-10*10
shade_percent <- tree_canopy_area/tile_area

expected_crowd <- function(shade_threshold) {
    ceiling(shade_threshold/shade_percent)
}

