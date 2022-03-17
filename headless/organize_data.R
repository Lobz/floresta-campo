# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

get_data <- function (myfilename) {
    data <- read.csv(file=myfilename,stringsAsFactors=T)
    data$edge_range <- data$circ.araucaria - data$circ.broadleaf
    data$inner_range <- data$circ05.araucaria - data$circ05.broadleaf
    data$noAr <- data$initial_pop_araucaria ==0
    data$noFi <- data$wildfire_rate ==0
    data$full <- !data$noAr & !data$noFi
    data$circ.max <- pmax(data$circ.araucaria, data$circ.broadleaf)
    return(data)
}

get_finalsteps <- function (data) {
    finalstep <- function (one.run) {
        subset(one.run, time == max(one.run$time))
    }

    finalvalues <- by(data, data$sim_unique_id, finalstep)
    finalvalues <- as.data.frame(do.call(rbind,finalvalues))

    finalvalues$extinction <- finalvalues$n.araucaria + finalvalues$n.broadleaf == 0
    finalvalues$area_limit <- finalvalues$circ.max >= 300
    finalvalues$time_limit <- finalvalues$time == 2000
    return(finalvalues)
}

get_statistics <- function (one.run) {

    ## extinction
    time.to.extinction.araucaria <- min(subset(one.run, n.araucaria==0)$time)
    time.to.extinction.broadleaf <- min(subset(one.run, n.broadleaf==0)$time)
    time.to.extinction <- max(time.to.extinction.araucaria, time.to.extinction.broadleaf)
    extinction <- !is.na(time.to.extinction)
    
    ## complete afforestation
    time.to.afforestation.araucaria <- min(subset(one.run, c.araucaria>=300)$time)
    time.to.afforestation.broadleaf <- min(subset(one.run, c.broadleaf==0)$time)
    time.to.afforestation <- max(time.to.afforestation.araucaria, time.to.afforestation.broadleaf)
    afforestation <- !is.na(time.to.afforestation)

    linear_growth_rate <- function(one.run, column.par) {
        tryCatch( {
        model <- lm(one.run[,column.par] ~ one.run$time)
        model$coefficients[2]
        },
        error=function(e) NA)
    }

    ## circs linear
    circ.araucaria.gr <- linear_growth_rate(one.run, "circ.araucaria")
    circ.broadleaf.gr <- linear_growth_rate(one.run, "circ.broadleaf")
    circ05.araucaria.gr <- linear_growth_rate(one.run, "circ05.araucaria")
    circ05.broadleaf.gr <- linear_growth_rate(one.run, "circ05.broadleaf")
    circ.max.gr <- linear_growth_rate(one.run, "circ.max")

    exponential_growth_rate <- function(one.run, column.par) {
        model <- lm(log(one.run[,column.par]) ~ one.run$time)
        model$coefficients[2]
    }

    ## ns log
    n.araucaria.gr <- tryCatch(exponential_growth_rate(one.run, "n.araucaria"), error=function(e)NA)
    n.broadleaf.gr <- tryCatch(exponential_growth_rate(one.run, "n.broadleaf"), error=function(e)NA)

    ## edge_range median

    edge_range.med <- median(one.run$edge_range)


    data.frame(
                circ.araucaria.gr, circ.broadleaf.gr, circ.max.gr,
                circ05.araucaria.gr, circ05.broadleaf.gr,
                n.araucaria.gr, n.broadleaf.gr, edge_range.med,
                extinction, time.to.extinction,
                )
}

## this function expects only one run per group (one scenario)
extract_statistics <- function(data) {

    # Add these:
    #         frequency of extinction
    #         mean time to extinction
    #         frequency of complete afforestation
    #         mean time to complete afforestation
    #         rate of expansion
    #         frequency of competitive exclusion of \emph{Araucaria}
    #         patterns of spatial distribution

    data$sim_unique_id <- as.factor(data$sim_unique_id)
    results.raw <- by(data,data$sim_unique_id, get_statistics)
    statistics <- as.data.frame(do.call(rbind,results.raw))

    statistics
}
