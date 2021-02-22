#xmlparse.R

library(XML)

myColNames <- c("n.araucaria", "n.broadleaf", "circ.araucaria", "circ.broadleaf", "firesize",
                "wildfire.rate", "initial.pop.araucaria", "initial.pop.broadleaf","shade.threshold.araucaria","shade.threshold.ratio")
simtypes <- c("Full","NoAr","NoFi")
#mydirname <- paste0("fc",finalstep,'_',numreps,"_output")
mydirname <- "test_outs"

filename <- function (prefix, rep) {
    paste0(mydirname,"/simulation-outputs",prefix,rep,".xml")
}

get.sim.name <- function(filepath) {
    filename <- basename(filepath)
    substring(filename,nchar("simulation-outputs")+1,nchar(filename) - nchar(".xml"))
}

get_my_data <- function(filepath) {
        data <- xmlToDataFrame(filepath)
        names(data) <- myColNames
        data$time <- 1:nrow(data) # allow for incomplete simulations
        data$sim_unique_id <- get.sim.name(filepath)
        data
}

my_full_col_names <- c(myColNames,"time","sim_unique_id")
dados <- data.frame(matrix(ncol=length(my_full_col_names),nrow=0))
names(dados) <- my_full_col_names

my_files <- list.files(mydirname, pattern=".*.xml", full.names=TRUE)
my_files

for (f in my_files) {
        print(f)
        data <- get_my_data(f)
        dados <- rbind(dados,data)
}
head(dados)
myfilename=paste0("data/",mydirname,"data.csv")
write.csv(dados,myfilename,row.names=FALSE)


