
source("organize_data.R")

data <- get_data(myfilename)

data<-subset(data,firesize>0)

results.raw <- by(data$firesize,data$grass_flammability, function(x) quantile(x, probs=c(.05,.25,.5,.75,.95)))
quantiles <- as.data.frame(do.call(rbind,results.raw))
names(quantiles) <- c("inf", "q1", "median", "q3", "sup")
srquantiles <- sqrt(quantiles)
quantiles<-quantiles/36.0

quantiles$gf <- as.numeric(rownames(quantiles))
srquantiles$gf<-quantiles$gf

## w error bars
batchname <- tools::file_path_sans_ext(basename(myfilename))
pdf(paste0("images/",batchname,"_firesize_full.pdf"), width=12, height=8)
plot(median ~ gf, quantiles, ylim=c(0,100), pch=20, xlab="grass flammability", ylab="fire size (percent of maximum area)")
arrows(x0=quantiles$gf, y0=quantiles$inf, x1=quantiles$gf, y1=quantiles$sup, code=3, angle=90, length=0.01)
title("Distribution of sizes of wildfires (grass only simulations)")
dev.off()
