
# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

source("plotsfuns.utils.R")
### PLOTS
datafull <- read.csv(file=myfilename,stringsAsFactors=T)

data <- datafull
data$edge_range <- data$circ.araucaria - data$circ.broadleaf
data$noAr <- data$initial_pop_araucaria ==0
data$noFi <- data$wildfire_rate ==0
data$full <- !data$noAr & !data$noFi
data$circ.max <- pmax(data$circ.araucaria, data$circ.broadleaf)

finalstep <- function (one.run) {
    subset(one.run, time == max(one.run$time))
}

finalvalues <- by(data, data$sim_unique_id, finalstep)
finalvalues <- as.data.frame(do.call(rbind,finalvalues))

finalvalues$extinction <- finalvalues$n.araucaria + finalvalues$n.broadleaf == 0
finalvalues$area_limit <- finalvalues$circ.max >= 300
finalvalues$time_limit <- finalvalues$time == 2000

plot_final_values(data,"wildfire_rate")

## this function tells which function has a higher (linear) growth rate
## returns 1, 2 or FALSE is the difference is insignificant
library(bbmle)
compare.two <- function (x1, t1, x2, t2, log=FALSE) {
    ## loglikelyhood functions for growthrate comparisons
    LL.same <- function(a1, a2, b, sd){
        -sum(dnorm(x1, mean= a1 + b*t1, sd=sd, log=T)) -sum(dnorm(x2, mean= a2 + b*t2, sd=sd, log=T))
    }

    LL.diff <- function(a1, a2, b1, b2, sd){
        -sum(dnorm(x1, mean= a1 + b1*t1, sd=sd, log=T)) -sum(dnorm(x2, mean= a2 + b2*t2, sd=sd, log=T))
    }

    if (log) {
        x1 <- log(x1)
        x2 <- log(x2)
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

test.hypotheses <- function(data) {
    ## separate scenarios
    NoAr <- subset(data,noAr)
    NoFi <- subset(data,noFi)
    Full <- subset(data,full)

    ## extract variables

    maxtime <- max(Full$time)
    Full.final <- subset(Full, time==max(Full$time))
    NoAr.final <- subset(NoAr, time==max(NoAr$time))
    NoFi.final <- subset(NoFi, time==max(NoFi$time))

    ## extinction full check
    ext_full_a <- Full.final$n.araucaria < 10
    ext_full_b <- Full.final$n.broadleaf < 10
    ext_full <- ext_full_a && ext_full_b
    ext_NoAr <- NoAr.final$n.araucaria + NoAr.final$n.broadleaf < 10
    ext_NoFi_a <- NoFi.final$n.araucaria < 10 
    ext_NoFi_b <- NoFi.final$n.broadleaf < 10
   
    ## hyp1 : patch will grow
    if (ext_full) {
        hyp1 <- FALSE
    }
    else {
        model <- lm(circ.max ~ time, Full)
        hyp1 <- coef(model)[2] > 0 && summary(model)$coefficients[2,4] < 0.05
    }
    ## hyp2 : araucaria is on the edge in full and NoFi models
    hyp2a <- median(Full$edge_range) > 10
    hyp2b <- median(NoFi$edge_range) > 10
    ## hyp3 : broadleaf grows better with araucaria / can't grow without araucaria
    if (!ext_full && ext_NoAr) {
        hyp3a <- TRUE
        hyp3b <- TRUE
    }
    else if (ext_full && !ext_NoAr) {
        hyp3a <- FALSE
        model_NoAr <- lm(circ.broadleaf ~ time, NoAr)
        growth_NoAr <- coef(model_NoAr)[2] > 0 && summary(model_NoAr)$coefficients[2,4] < 0.05
        hyp3b <- !growth_NoAE
    }
    else {
        aic <- compare.two(Full$circ.broadleaf, Full$time, NoAr$circ.broadleaf, NoAr$time)
        hyp3a <- (aic == 1)

        model_NoAr <- lm(circ.broadleaf ~ time, NoAr)
        growth_NoAr <- coef(model_NoAr)[2] > 0 && summary(model_NoAr)$coefficients[2,4] < 0.05
        hyp3b <- !growth_NoAr
    }
    ## hyp4 : without fire, broadleaf grows and competes with araucaria
    if (ext_full_b && !ext_NoFi_b) {
        hyp4a <- TRUE
    }
    else {
        aic <- compare.two(Full$circ.broadleaf, Full$time, NoFi$circ.broadleaf, NoFi$time)
        hyp4a <- (aic == 2)
    }
    if (!ext_full_a && ext_NoFi_a) {
        hyp4b <- TRUE
    }
    else if (ext_full_a && !ext_NoFi_a) {
        hyp4b <- FALSE
    }
    else {
        aic <- compare.two(Full$n.araucaria, Full$time, NoFi$n.araucaria, NoFi$time, log=TRUE)
        hyp4b <- (aic == 1)
    }
    ## hyp5 : broadleaf expansion tracks araucaria expansion
    if (ext_full_a || ext_full_b) {
        hyp5 <- FALSE
    }
    else {
        model <- lm(circ.broadleaf ~ time, Full)
        hyp5 <- hyp2a && coef(model)[2] > 0 && summary(model)$coefficients[2,4] < 0.05
    }

    ## array of hypotheses
    hypotheses <- c(hyp1,hyp2a,hyp2b,hyp3a,hyp3b,hyp4a,hyp4b,hyp5,ext_full,ext_NoAr,ext_NoFi_a && ext_NoFi_b)
    names(hypotheses) <- c("h1","h2a","h2b","h3a","h3b","h4a","h4b","h5","Full_extinction","NoAr_extinction","NoFi_extinction")
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
onetimestep<- subset(data,time==1)
results_par <- merge(results,onetimestep,by="par_group")
data_hyps<- merge(data,results,by="par_group")

plot_final_values(subset(finalvalues,full),"araucaria_base_flammability")
plot_all_hyps(results_par,"grass_flammability")
plot.hypotheses(results[!results$full_extinction,],function(x,c,...) barplot(table(x[,c]),...))
plot.hyp(results_par,"Full_extinction","wildfire_rate")
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

