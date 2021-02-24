#createxml.utils.R

header <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<Experiment_plan>
"
outputs <- readChar("outputs.xmlpart", file.info("outputs.xmlpart")$size)

simheadbeg <- '
    <Simulation id="'
simheadend <- paste0('" sourcePath="',gamlfile,'" finalStep="',finalstep,'" experiment="fireandforest">')
footer <- '</Experiment_plan>'

w <- function (str) {
    write(str,file=filename,append=T)
}

n <- samplesize*numreps
chars <- c(LETTERS,letters,0:9)
rndc <- do.call(paste0, replicate(5, sample(chars, n , TRUE), FALSE)) # generate unique ids


par.line <- function(name,p) {
  paste0('<Parameter name="',name,'" type="FLOAT" value="',p,'" />')
}

par.row <- function(row) {
  paste(par.line(names(row),row),collapse="\n")
}
