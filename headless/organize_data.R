# make sure you've used createxml to create the xml for gama_headless and xmlToCsv to save the results in csv format
CONST_ARENA_RADIUS <- 300

my_writecsv <- function(data, filename) {
    write.csv(data,filename, row.names=F, fileEncoding="utf-8", na="")
}

get_data <- function (myfilename) {
    data <- read.csv(file=myfilename,stringsAsFactors=T)
    data$edge_range <- data$rad95A - data$rad95B
    data$inner_range <- data$circ05.araucaria - data$circ05.broadleaf
    data$noAr <- data$initial_pop_araucaria ==0
    data$noFi <- data$wildfire_rate ==0
    data$full <- !data$noAr & !data$noFi
    data$circ.max <- pmax(data$rad95A, data$rad95B)
    data$scenario <- factor(data$full + 2*data$noAr + 3*data$noFi, labels=c("full", "noAr", "noFi"))
    return(data)
}

finalstep <- function (one.run) {
    subset(one.run, time == max(one.run$time))
}

get_finalsteps <- function (data, time_limit) {
    finalvalues <- by(data, data$sim_unique_id, finalstep)
    finalvalues <- as.data.frame(do.call(rbind,finalvalues))

    finalvalues$extinction <- finalvalues$nA + finalvalues$nB == 0
    finalvalues$area_limit <- finalvalues$circ.max >= CONST_ARENA_RADIUS
    finalvalues$time_limit <- finalvalues$time == time_limit & !finalvalues$extinction & !finalvalues$area_limit
    finalvalues$extinction_araucaria <- finalvalues$nA == 0 & !finalvalues$extinction
    return(finalvalues)
}

    linear_growth_rate <- function(one.run, column.par) {
        tryCatch( {
        model <- lm(one.run[,column.par] ~ one.run$time)
        model$coefficients[2]
        },
        error=function(e) NA)
    }

    exponential_growth_rate <- function(one.run, column.par) {
        model <- lm(log(one.run[,column.par]) ~ one.run$time)
        model$coefficients[2]
    }

get_statistics <- function (one.run) {
    time.to.extinctionA <- min(one.run$time[one.run$nA == 0])
    time.to.extinction <- min(one.run$time[one.run$nA + one.run$nB == 0])
    time.to.afforestation <- min(one.run$time[one.run$circ.max >= CONST_ARENA_RADIUS])

    ## circs linear
    rad95A.gr <- linear_growth_rate(one.run, "rad95A")
    rad95B.gr <- linear_growth_rate(one.run, "rad95B")
    circ05.araucaria.gr <- linear_growth_rate(one.run, "circ05.araucaria")
    circ05.broadleaf.gr <- linear_growth_rate(one.run, "circ05.broadleaf")
    circ.max.gr <- linear_growth_rate(one.run, "circ.max")

    ## ns log
    nA.gr <- tryCatch(exponential_growth_rate(one.run, "nA"), error=function(e)NA)
    nB.gr <- tryCatch(exponential_growth_rate(one.run, "nB"), error=function(e)NA)

    ## edge_range median

    edge_range.med <- median(one.run$edge_range)
    edge_range.max <- max(one.run$edge_range)
    edge_range.min <- min(one.run$edge_range)
    inner10A.med <- median(one.run$inner10A)
    inner10A.max <- max(one.run$inner10A)
    inner10A.min <- min(one.run$inner10A)


    sim_unique_id <- one.run$sim_unique_id[1]
    data.frame(
                rad95A.gr, rad95B.gr, circ.max.gr,
                circ05.araucaria.gr, circ05.broadleaf.gr,
                nA.gr, nB.gr,
                time.to.extinctionA, time.to.afforestation, time.to.extinction,
                edge_range.med, edge_range.max, edge_range.min,
                inner10A.med, inner10A.max, inner10A.min,
                sim_unique_id
            )
}

extract_statistics <- function(data, time_limit=max(data$time)) {

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

    finalvalues <- get_finalsteps(data, time_limit)

    merge(statistics,finalvalues, by="sim_unique_id")
}

minmax <- function (x) {
    x <- as.numeric(x)
    m <- round(min(x,na.rm=T),digits=2)
    M <- round(max(x,na.rm=T),digits=2)
    paste0(m," - ",M)
}
library(scales)
basic_statistics <- function(statistics) {
    statistics[statistics==Inf] <- NA
    r1 <- sapply(statistics,minmax)
    r2 <- percent(sapply(statistics[,c("extinction", "area_limit", "time_limit", "extinction_araucaria")], mean))
    c(r2,r1)
}

### latex
library(stringi)
latex_table <- function (data, filename, ns=names(data), caption="") {
	data[is.na(data)] <- ''
	rows <- paste0("\t",lapply(as.data.frame(t(data)),paste,collapse=' & '),'\\\\')
	firstrow <- paste0("\t", paste(ns, collapse = ' & '), '\\\\\n')
	body <- paste0(rows, collapse = "\n")
	table <- paste0("\n\\begin{longtblr}",
		"[caption = {", caption, "}]",
		"{hlines, vlines, rowhead = 1, row{1} = {font=\\bfseries}}\n",
		firstrow,
		body,
		"\n\\end{longtblr}\n"
		)

    table <- gsub('_',' ',table)
    table <- gsub('í','\\\\\'i',table)
    table <- gsub('ó','\\\\\'o',table)
    table <- gsub('#','\\\\#',table)
    table <- gsub('%','\\\\%',table)
    table <- stri_enc_toutf8(table)

    cat(table, file=filename)
}