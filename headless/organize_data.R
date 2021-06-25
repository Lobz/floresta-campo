# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format

get_data <- function (myfilename) {
    data <- read.csv(file=myfilename,stringsAsFactors=T)
    data$edge_range <- data$circ.araucaria - data$circ.broadleaf
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

extract_statistics <- function(data) {
    get_statistics <- function (one.run) {

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
        circ.max.gr <- linear_growth_rate(one.run, "circ.max")

        exponential_growth_rate <- function(one.run, column.par) {
            model <- lm(log(one.run[,column.par]) ~ one.run$time)
            model$coefficients[2]
        }

        ## ns log
        n.araucaria.gr <- tryCatch(exponential_growth_rate(one.run, "n.araucaria"), error=function(e)NA)

        ## edge_range median

        edge_range.med <- median(one.run$edge_range)

        data.frame(circ.araucaria.gr, circ.broadleaf.gr, circ.max.gr, n.araucaria.gr, edge_range.med)
    }

    ## circ.max gr linear full
    ## median adgerange full
    ## median edgerange nofi
    ## circ b gr noar
    ## circ b gr full
    ## circ b gr (full - noar)
    ## circ b gr (nofi - full)
    ## n a gr log (full - nofi)
    
    join_scenarios <- function (one.group) {
        ## separate scenarios
        NoAr <- get_statistics(subset(one.group,noAr))
        NoFi <- get_statistics(subset(one.group,noFi))
        Full <- get_statistics(subset(one.group,full))
        par_group <- as.character(one.group$par_group[1])

        data.frame (par_group, Full$circ.max.gr, Full$edge_range.med, NoFi$edge_range.med, NoAr$circ.broadleaf.gr, Full$circ.broadleaf.gr, NoFi$circ.broadleaf.gr, Full$n.araucaria.gr, NoFi$n.araucaria.gr)
    }

    data$par_group <- as.factor(data$par_group)

    results.raw <- by(data,data$par_group, join_scenarios)
    statistics <- as.data.frame(do.call(rbind,results.raw))
    statistics$circ.FullminusNoAr <- statistics$Full.circ.broadleaf.gr - statistics$NoAr.circ.broadleaf.gr 
    statistics
}
