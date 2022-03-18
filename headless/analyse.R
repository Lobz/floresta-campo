
source("headless/organize_data.R")
source("headless/test_hypothesis.R")
source("headless/plotsfuns.utils.R")
### script
##myfilename<-
data <- get_data(myfilename)
groupname <- substring(myfilename,nchar("data/")+1,nchar(myfilename) - nchar("_data.csv"))
summary(data)
length(unique(data$sim_unique_id)) # number of sims

## image dir
imagedir <- paste0("images/",groupname,"/")
dir.create(imagedir)

### several per-sim statistics
statistics <- extract_statistics(data, time_limit=max(data$time))
summary(statistics)

### join back to lhs object
load(paste0("data/",groupname,".RData"))
my_LHS_pars$N ## number of pargroups
summary(my_LHS_pars$data)
library(pse)
## example
myLHS<-tell(my_LHS_pars, statistics$circ.broadleaf.gr, nboot=30)

### lhs plots
plotecdf(myLHS, stack=TRUE)
plotscatter(myLHS)
plotprcc(myLHS)


plot.fours.columns(data, plot.scenarios)


### hypothesis testing
results<- test.hypotheses.all(data)
results$par_group<-as.numeric(rownames(results))
### merge back into values of data
results_par <- merge(results,statistics,by="par_group")
data_hyps<- merge(data,results,by="par_group")

plot_final_values(subset(statistics,full),"grass_flammability")
plot_all_hyps(results_par,"grass_flammability")

save.plots <- function(name) {
    pdf(paste0(imagedir,name,".pdf"), width=12, height=8)
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
example <- subset(full, sim_unique_id==id_ex, select=c(circ05.araucaria, circ.broadleaf, circ.araucaria, time))
ex <- aggregate(full, by=list(full$time), mean)
#ex <- aggregate(example, by=list(example$time), mean)
n <- nrow(ex)

barlengths <- matrix(c(ex$circ05.araucaria, ex$circ.broadleaf, ex$circ.araucaria), nrow=3, byrow=T)
colnames(barlengths) <- ex$time
barlengths[3,] <- barlengths[3,] - barlengths[2,]
barlengths[2,] <- barlengths[2,] - barlengths[1,]

pdf(paste0("images/arealengths_",groupname,"_average.pdf"), width=7, height=5)
par(lwd=3)
barplot(height=barlengths, border=F, space=0, col=c("purple", "purple", "darkgreen"), 
        angle=c(90,45,90), density=c(100,20,100),
        axes=T, ylab="average meters from center", xlab="years",
        legend.text=c("broadleaf dominance area (interior)", "coexistence area", "araucaria dominance area (edge)"),
        args.legend=list(x="topleft", border=F, bty = "n"))
dev.off()

# just for fun, do the same with NoFi
nofi <- subset(data, noFi, select=c(circ05.araucaria, circ.broadleaf, circ.araucaria, time))
summary(nofi)
id_ex <- unique(nofi$sim_unique_id)[10]
example <- subset(nofi, sim_unique_id==id_ex, select=c(circ05.araucaria, circ.broadleaf, circ.araucaria, time))

ex <- aggregate(nofi, by=list(nofi$time), mean)
#ex <- aggregate(example, by=list(example$time), mean)
n <- nrow(ex)

barlengths <- matrix(c(ex$circ05.araucaria, ex$circ.broadleaf, ex$circ.araucaria), nrow=3, byrow=T)
colnames(barlengths) <- ex$time
barlengths[3,] <- barlengths[3,] - barlengths[2,]
barlengths[2,] <- barlengths[2,] - barlengths[1,]

pdf(paste0("images/arealengths",groupname,"_average_NoFi.pdf", width=7, height=5)
par(lwd=3)
barplot(height=barlengths, border=F, space=0, col=c("purple", "purple", "darkgreen"), 
        angle=c(90,45,90), density=c(100,20,100),
        axes=T, ylab="average meters from center", xlab="years",
        legend.text=c("broadleaf dominance area (interior)", "coexistence area", "araucaria dominance area (edge)"),
        args.legend=list(x="topleft", border=F, bty = "n"))
dev.off()

## area lengths comparison one timestep boxplots

t <- 700

v <- subset(data,!noAr & time==t)
summary(v)
boxplot(edge_range~full, v, xlab="wildfires")
boxplot(inner10A~full, v, xlab="wildfires")
