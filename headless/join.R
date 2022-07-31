filename1 <- "data/LHS_Mar182022_YapWS_data.csv"
filename2 <- "data/LHS_Mar192022_fhwTe_data.csv"


source("headless/organize_data.R")


data1 <- get_data(filename1)
data2 <- get_data(filename2)

summary(unique(data1$par_group)) # number of groups
summary(unique(data2$par_group)) # number of groups

data2$par_group <- data2$par_group + 100 # unique parameter groups

data <- rbind(data1,data2)

length(unique(data$sim_unique_id)) # number of sims

summary(unique(data$par_group)) # number of groups
subset(data,par_group==1 & time==1)

summary(data)

head(data)

my_writecsv(data,"data/LHS_merged_data.csv")
