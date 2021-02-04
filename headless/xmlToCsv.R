#xmlparse.R

library(XML)

myColNames <- c("n.araucaria", "n.broadleaf", "circ.araucaria", "circ.broadleaf", "firesize")
simtypes <- c("Full","NoAr","NoFi")
mydirname <- paste0("fc",finalstep,'_',numreps,"_output")

filename <- function (prefix, rep) {
    paste0(mydirname,"/simulation-outputs",prefix,rep,".xml")
}

get_my_data <- function(type, i) {
        data <- xmlToDataFrame(filename(type,i))
        names(data) <- myColNames
        data$time <- 1:nrow(data) # allow for incomplete simulations
        data$scenario <- type
        data$rep_num <- i
        data$sim_unique_id <- paste0(type,i)
        data
}

dados <- data.frame(matrix(ncol=9,nrow=0))
names(dados) <- c(myColNames,"time","scenario","rep_num","sim_unique_id")

for (i in 1:numreps) {
    for(type in simtypes) {
        cat(i)
        print(type)
        data <- get_my_data(type,i)
        dados <- rbind(dados,data)
    }
}

write.csv(dados,file=paste0(mydirname,"/data.csv"),row.names=FALSE)


