#xmlparse.R

library(XML)


myColNames <- c("par_group","n.araucaria", "n.broadleaf", "circ.araucaria", "circ.broadleaf",
                    "firesize","wildfire_rate", "initial_pop_araucaria", "initial_pop_broadleaf",
                    "shade_threshold_araucaria", "shade_threshold_ratio","araucaria_fire_tolerance",
                    "araucaria_dispersal", "broadleaf_dispersal","grass_flammability",
                    "circ05.araucaria", "circ05.broadleaf",
                    "rad50A", "rad50B", "inner10A", "inner10B", "outer10A", "outer10B"
                    # ,"araucaria_growthrate_0", "broadleaf_growthrate_0",
                    # "araucaria_growthrate_1", "broadleaf_growthrate_1",
                    # "araucaria_growthrate_2", "broadleaf_growthrate_2",
                    # "araucaria_growthrate_3", "broadleaf_growthrate_3"
                    )

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
    }, error = function(e){NULL}) # nolint
}

my_files <- list.files(mydirname, pattern=".*.xml", full.names=TRUE)
length(my_files)
#my_files <- Filter(file.notempty,my_files)
#length(my_files)

dados_list <- lapply(my_files,get_my_data)
length(dados_list)
sapply(dados_list,length)
dados <- do.call(rbind, dados_list)
dim(dados)

groupname <- paste0(strsplit(dados$sim_unique_id[1],'_')[[1]][1:3],collapse='_')
head(dados)
myfilename <- paste0("data/", groupname, "_data.csv")
write.csv(dados, myfilename, row.names = FALSE)
