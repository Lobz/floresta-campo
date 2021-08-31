
source("organize_data.R")
source("test_hypothesis.R")
source("plotsfuns.utils.R")
### script
data <- get_data(myfilename)
summary(data)
plot.fours.columns(data, plot.scenarios)

statistics_full <- extract_statistics(subset(data,full))
statistics_noAr <- extract_statistics(subset(data,noAr))
statistics_noFi <- extract_statistics(subset(data,noFi))

statistics_full_minus_noAr <- statistics_full - statistics_noAr
statistics_noFi_minus_noAr <- statistics_noFi - statistics_noAr
statistics_full_minus_noFi <- statistics_full - statistics_noFi


finalvalues <- get_finalsteps(data)
summary(finalvalues)
### join back to lhs object
load(mylhsfilename)
library(pse)
myLHS<-tell(my_LHS_pars, statistics_noFi$circ05.broadleaf.gr, nboot=30)

### lhs plots
plotecdf(myLHS, stack=TRUE)
plotscatter(myLHS)
plotprcc(myLHS)


### hypothesis testing
results<- test.hypotheses.all(data)
results$par_group<-as.numeric(rownames(results))
### merge back into values of data
results_par <- merge(results,finalvalues,by="par_group")
data_hyps<- merge(data,results,by="par_group")

plot_final_values(subset(finalvalues,full),"grass_flammability")
plot_all_hyps(results_par,"grass_flammability")

batchname <- tools::file_path_sans_ext(basename(myfilename))
save.plots <- function(name) {
    pdf(paste0("images/",batchname,"/",name,".pdf"), width=12, height=8)
    plot_all_hyps(results_par,name)
    dev.off()
}
lapply(par_names,save.plots)

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

# plot inner, mid, edge areas in a simulation

full <- subset(data, full)
summary(full)
id_ex <- full$sim_unique_id[1]
example <- subset(full, sim_unique_id==id_ex, select=c(circ05.araucaria, circ.broadleaf, edge_range, time))

ex <- aggregate(full, by=list(full$time), mean)
n <- nrow(ex)

barlengths <- matrix(c(ex$circ05.araucaria, ex$circ.broadleaf - ex$circ05.araucaria, ex$edge_range), nrow=3, byrow=T)
colnames(barlengths) <- ex$time
barplot(height=barlengths, border=F, space=0, col=c("purple", "purple", "darkgreen"), 
        angle=c(90,45,0), density=c(100,20,100),
        axes=T, ylab="average meters from center", xlab="years",
        legend.text=c("broadleaf dominance area (interior)", "coexistance area", "araucaria dominance area (edge)"),
        args.legend=list(x="topleft"))
