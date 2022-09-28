source("headless/organize_data.R")
source("headless/test_hypothesis.R")
source("headless/plotsfuns.utils.R")
### script
##myfilename<-
data <- get_data(myfilename)
data$tree_dispersal <- data$araucaria_dispersal
groupname <- substring(myfilename,nchar("data/")+1,nchar(myfilename) - nchar("_data.csv"))
summary(data)
length(unique(data$sim_unique_id)) # number of sims
length(unique(data$par_group)) # number of groups
names(data) # check outputs

## image dir
imagedir <- paste0("images/",groupname,"/")
dir.create(imagedir)

### several per-sim statistics
statistics <- extract_statistics(data, time_limit=max(data$time))
summary(statistics)

### join back to lhs object
load(paste0("data/",groupname,".RData"))
my_LHS_pars$N ## number of pargroupso
parameters <- my_LHS_pars$factors
summary(my_LHS_pars$data)
library(pse)


##############################################################################################

# basic statistics:
#
# • frequency of extinction
# • mean time to extinction
# • frequency of complete afforestation
# • mean time to complete afforestation
# • frequency of competitive exclusion of Araucaria
# • patterns of spatial distribution

statsFull <- subset(statistics,full)
statsnoAr <- subset(statistics,noAr)
statsnoFi <- subset(statistics,noFi)

bstats <- basic_statistics(statsFull)
bstats <- cbind(bstats, basic_statistics(statsnoAr))
bstats <- cbind(bstats, basic_statistics(statsnoFi))
bstats <- as.data.frame(bstats)
bstats <- cbind(rownames(bstats),bstats)
names(bstats) <- c("","full","noAr","noFi")
str(bstats)
bstats[,2:4]

latex_table(bstats, paste0("tabela_bstats_",groupname,".tex"))

### nice place for a image save/load
save.image('./data/scenarios_dQSm9.RData')

## example
myLHS<-tell(my_LHS_pars, statsnoAr$circ.max.gr, nboot=30)

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

plot_final_values(statsFull,"grass_flammability")
plot_all_hyps(results_par,"grass_flammability")

save.plots <- function(name) {
    pdf(paste0(imagedir,name,"-hyps.pdf"), width=12, height=8)
    plot_all_hyps(results_par,name)
    dev.off()
}
lapply(my_LHS_pars$factors,save.plots)

save.plots.dyn <- function(name) {
    pdf(paste0(imagedir,name,"-full-dynamics.pdf"), width=12, height=8)
    plot.fours.columns(subset(data,full),lines.par,column.par=name)
    dev.off()
    pdf(paste0(imagedir,name,"-noAr-dynamics.pdf"), width=12, height=8)
    plot.fours.columns(subset(data,noAr),lines.par,column.par=name)
    dev.off()
    pdf(paste0(imagedir,name,"-noFi-dynamics.pdf"), width=12, height=8)
    plot.fours.columns(subset(data,noFi),lines.par,column.par=name)
    dev.off()
}
lapply(my_LHS_pars$factors,save.plots.dyn)

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

### AREA LENGTHS
# plot inner, mid, edge areas
full <- subset(data, full)
full_means <- aggregate(full, by=list(full$time), mean)
filename <- paste0(imagedir,"arealengths_average_full.pdf")
arealengths(filename, full_means, "Evolution of species dominace areas (Full)")
# just for fun, do the same with NoFi
nofi <- subset(data, noFi, select=c(circ05.araucaria, rad95B, rad95A, time))
nofi_means <- aggregate(nofi, by=list(nofi$time), mean)
filename <- paste0(imagedir,"arealengths_average_nofi.pdf")
arealengths(filename, nofi_means, "Evolution of species dominace areas (NoFi)")


## area lengths comparison one timestep boxplots
t <- 700

v <- subset(data, !noAr & time==t)
v <- subset(statistics, !noAr)
summary(v)
filename <- paste0(imagedir,"edge_range700")
boxplot(edge_range~full, v, xlab="Wildfires", main="Length of Araucaria-dominated edge (LHS/700y)", ylab="rad95A-rad95B (m)")
savePlot(filename,"png")
filename <- paste0(imagedir,"inner10A700")
boxplot(inner10A~full, v, xlab="Wildfires", main="Radius of broadleaf-dominated interior (LHS/700y)", ylab="radius of circle containing 10 or less A. (m)")
savePlot(filename,"png")
