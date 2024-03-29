
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
    ext_full_a <- Full.final$nA < 10
    ext_full_b <- Full.final$nB < 10
    ext_full <- ext_full_a && ext_full_b
    ext_NoAr <- NoAr.final$nA + NoAr.final$nB < 10
    ext_NoFi_a <- NoFi.final$nA < 10
    ext_NoFi_b <- NoFi.final$nB < 10

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
        model_NoAr <- lm(rad95B ~ time, NoAr)
        growth_NoAr <- coef(model_NoAr)[2] > 0 && summary(model_NoAr)$coefficients[2,4] < 0.05
        hyp3b <- !growth_NoAr
    }
    else {
        aic <- compare.two(Full$rad95B, Full$time, NoAr$rad95B, NoAr$time)
        hyp3a <- (aic == 1)

        model_NoAr <- lm(rad95B ~ time, NoAr)
        growth_NoAr <- coef(model_NoAr)[2] > 0 && summary(model_NoAr)$coefficients[2,4] < 0.05
        hyp3b <- !growth_NoAr
    }
    ## hyp4 : without fire, broadleaf grows and competes with araucaria
    if (ext_full_b && !ext_NoFi_b) {
        hyp4a <- TRUE
    }
    else {
        aic <- compare.two(Full$rad95B, Full$time, NoFi$rad95B, NoFi$time)
        hyp4a <- (aic == 2)
    }
    if (!ext_full_a && ext_NoFi_a) {
        hyp4b <- TRUE
    }
    else if (ext_full_a && !ext_NoFi_a) {
        hyp4b <- FALSE
    }
    else {
        aic <- compare.two(Full$nA, Full$time, NoFi$nA, NoFi$time, log=TRUE)
        hyp4b <- (aic == 1)
    }
    ## hyp5 : broadleaf expansion tracks araucaria expansion
    if (ext_full_a || ext_full_b) {
        hyp5 <- FALSE
    }
    else {
        model <- lm(rad95B ~ time, Full)
        hyp5 <- hyp2a && coef(model)[2] > 0 && summary(model)$coefficients[2,4] < 0.05
    }

    ## array of hypotheses
    hypotheses <- c(hyp1,hyp2a,hyp2b,hyp3a,hyp3b,hyp4a,hyp4b,hyp5,ext_full,ext_NoAr,ext_NoFi_a && ext_NoFi_b)
    names(hypotheses) <- c("h1","h2a","h2b","h3a","h3b","h4a","h4b","h5","Full_extinction","NoAr_extinction","NoFi_extinction")
    hypotheses
}

test.hypotheses.noexts <- function(data) {
    ## separate scenarios
    NoAr <- subset(data,noAr)
    NoFi <- subset(data,noFi)
    Full <- subset(data,full)

    ## hyp1 : patch will grow
    model <- lm(circ.max ~ time, Full)
    hyp1 <- coef(model)[2] > 0 && summary(model)$coefficients[2,4] < 0.05
    ## hyp2 : araucaria is on the edge in full and NoFi models
    hyp2a <- median(Full$edge_range) > 10
    hyp2b <- median(NoFi$edge_range) > 10
    ## hyp3 : broadleaf grows better with araucaria / can't grow without araucaria
    aic <- compare.two(Full$rad95B, Full$time, NoAr$rad95B, NoAr$time)
    hyp3a <- (aic == 1)

    model_NoAr <- lm(rad95B ~ time, NoAr)
    growth_NoAr <- coef(model_NoAr)[2] > 0 && summary(model_NoAr)$coefficients[2,4] < 0.05
    hyp3b <- !growth_NoAr
    ## hyp4 : without fire, broadleaf grows and competes with araucaria
    aic <- compare.two(Full$rad95B, Full$time, NoFi$rad95B, NoFi$time)
    hyp4a <- (aic == 2)
    aic <- compare.two(Full$nA, Full$time, NoFi$nA, NoFi$time, log=TRUE)
    hyp4b <- (aic == 1)
    ## hyp5 : broadleaf expansion tracks araucaria expansion
    model <- lm(rad95B ~ time, Full)
    hyp5 <- hyp2a && coef(model)[2] > 0 && summary(model)$coefficients[2,4] < 0.05

    ## array of hypotheses
    hypotheses <- c(hyp1,hyp2a,hyp2b,hyp3a,hyp3b,hyp4a,hyp4b,hyp5)
    names(hypotheses) <- c("h1","h2a","h2b","h3a","h3b","h4a","h4b","h5")
    hypotheses
}

test.hypotheses.all <- function(data) {
    ## turn indices into factors
    data$par_group <- as.factor(data$par_group)

    results.raw <- by(data,data$par_group,function(x){tryCatch(test.hypotheses(x), error=function(x) {rep(NA,11)} )})
    results <- as.data.frame(do.call(rbind,results.raw))
    results
}