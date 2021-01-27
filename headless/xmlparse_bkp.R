#xmlparse.R

library(XML)

myColNames <- c("n.araucaria", "n.broadleaf", "circ.araucaria", "circ.broadleaf", "firesize")
times <- 1:1000

dadosFM <- xmlToDataFrame("output-headless/simulation-outputsFull Model.xml")
names(dadosFM)<- myColNames

dadosNA <- xmlToDataFrame("output-headless/simulation-outputsNo Araucaria.xml")
names(dadosNA)<- myColNames

dadosNF <- xmlToDataFrame("output-headless/simulation-outputsNo Fire.xml")
names(dadosNF)<- myColNames

compare.slopes <- function(varname) {
    ymax <- max(as.numeric(c(dadosNF[,varname],dadosFM[,varname],dadosNA[,varname])))
    ymin <- min(as.numeric(c(dadosNF[,varname],dadosFM[,varname],dadosNA[,varname])))
    plot(dadosNF[,varname] ~ times, lty=3, type='l', ylim=c(ymin,ymax))
    lines(dadosFM[,varname] ~ times, lty=1)
    lines(dadosNA[,varname] ~ times, lty=2)
    
    
    slNF <- lm(dadosNF[,varname] ~ times)$coefficients[2]
    slFM <- lm(dadosFM[,varname] ~ times)$coefficients[2]
    slNA <- lm(dadosNA[,varname] ~ times)$coefficients[2]

    legend("topleft", legend=c(paste0("FM slope = ", round( slFM, 4)), paste0("NA slope = ", round( slNA, 4)), paste0("NF slope = ", round( slNF, 4))), lty=c(1,2,3))
}

compare.slopes("circ.broadleaf")
compare.slopes("circ.araucaria")

compare.slopes("n.broadleaf")
compare.slopes("n.araucaria")

dadosFM$edge.dist <- as.numeric(dadosFM$circ.araucaria) - as.numeric(dadosFM$circ.broadleaf)
dadosNF$edge.dist <- as.numeric(dadosNF$circ.araucaria) - as.numeric(dadosNF$circ.broadleaf)
dadosNA$edge.dist <- as.numeric(dadosNA$circ.araucaria) - as.numeric(dadosNA$circ.broadleaf)

compare.slopes("edge.dist")
