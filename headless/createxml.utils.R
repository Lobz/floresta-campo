#createxml.utils.R

header <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<Experiment_plan>
"
outputs <- readChar("outputs.xmlpart", file.info("outputs.xmlpart")$size)

simheadbeg <- '
    <Simulation id="'
simheadend <- paste0('" sourcePath="',gamlfile,'" finalStep="',finalstep,'" experiment="fireandforest" until="nb_araucaria + nb_broadleaf = 0">')
footer <- '</Experiment_plan>'

w <- function (str) {
    write(str,file=my_filename,append=T)
}

par.line <- function(name,p) {
  paste0('<Parameter name="',name,'" type="FLOAT" value="',p,'" />')
}

par.row <- function(row) {
  paste(par.line(names(row),row),collapse="\n")
}

chars <- c(LETTERS,letters,0:9)
