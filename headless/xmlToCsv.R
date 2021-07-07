#xmlparse.R

library(XML)


my_outputnames <- c("par_group","n.araucaria", "n.broadleaf", "circ.araucaria", "circ.broadleaf", "firesize")
my_parameternames <- c("wildfire_rate", "initial_pop_araucaria", "initial_pop_broadleaf", "shade_threshold_araucaria", "shade_threshold_ratio","araucaria_base_flammability","tree_dispersal", "tree_dispersal2","grass_flammability")

myColNames <- c(my_outputnames,my_parameternames)
mydirname <- outputdir

get.sim.name <- function(filepath) {
    filename <- basename(filepath)
    substring(filename,nchar("simulation-outputs")+1,nchar(filename) - nchar(".xml"))
}

file.notempty <- function(filenames) !file.info(filenames)$size == 0

get_my_data <- function(filepath) {
    print(filepath)
    tryCatch({
        data <- xmlToDataFrame(filepath)
        names(data) <- myColNames
        data$time <- 1:nrow(data) # allow for incomplete simulations
        data$sim_unique_id <- get.sim.name(filepath)
        data
    }, error= function(e){NULL})
}

my_files <- list.files(mydirname, pattern=".*.xml", full.names=TRUE)
my_files <- Filter(file.notempty,my_files)

dados_list <- lapply(my_files,get_my_data)

dados <- do.call(rbind, dados_list)

head(dados)
myfilename=paste0("data/",groupname,"_data.csv")
write.csv(dados,myfilename,row.names=FALSE)


