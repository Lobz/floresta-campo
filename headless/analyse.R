
source("organize_data.R")
source("test_hypothesis.R")
source("plotsfuns.utils.R")
### script
data <- get_data(myfilename)

statistics_raw <- extract_statistics(data)
statistics_raw$par_group <- as.numeric(statistics_raw$par_group) 
finalvalues <- get_finalsteps(data)
statistics <- merge(statistics_raw, subset(finalvalues, full), by="par_group")

### join back to lhs object
load(mylhsfilename)
library(pse)
myLHS<-tell(my_LHS_pars, statistics_raw)

results<- test.hypotheses.all(data)
results$par_group<-as.numeric(rownames(results))
### merge back into values of data
results_par <- merge(results,finalvalues,by="par_group")
data_hyps<- merge(data,results,by="par_group")

plot_final_values(subset(finalvalues,full),"grass_flammability")
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

