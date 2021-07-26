#createxml.utils.R

today <- function() paste0(strsplit(date()," ")[[1]][c(2:3,5)],collapse="")

par.line <- function(name,p) {
    paste0('<Parameter name="',name,'" type="FLOAT" value="',p,'" />')
}

par.row <- function(row) {
    paste(par.line(names(row),row),collapse="\n")
}

chars <- c(LETTERS,letters,0:9)


### MAIN FUNCTION

createxml <- function(par.data, groupname, scenarios=TRUE, stop.at.extinction=TRUE, stop.at.area.limit=TRUE, numreps=1) {

    header <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<Experiment_plan>
"
    outputs <- readChar("outputs.xmlpart", file.info("outputs.xmlpart")$size)

    simheadbeg <- '
        <Simulation id="'
    until=''
    if(stop.at.extinction && stop.at.area.limit) {
        until='until="length(araucaria) + length(broadleaf) = 0 or max(rad_broadleaf.y, rad_araucaria.y) > landscape_size/2"'
    }
    else if(stop.at.extinction) {
        until='until="length(araucaria) + length(broadleaf) = 0"'
    }
    else if(stop.at.area.limit) {
        until='until="max(rad_broadleaf.y, rad_araucaria.y) > landscape_size/2"'
    }
    simheadend <- paste0('" sourcePath="', gamlfile, '" finalStep="', finalstep, '" experiment="fireandforest" ', until, ' >')
    
    footer <- '</Experiment_plan>'

    ### Parameters
    sim.params<-rep(apply(par.data,1,par.row), numreps)
    ## scenarios
    if (scenarios) {
        FMparams <- paste0(sim.params,'')
        NAparams <- paste0(sim.params,'<Parameter name="initial_pop_ratio" type="INT" value="0" />')
        NFparams <- paste0(sim.params,'<Parameter name="Wildfires" type="BOOLEAN" value="false" />')

        my_names <- c("Full","NoAr","NoFi")
        sim.params <- c(FMparams,NAparams,NFparams)
    }

    ## UNIQUE IDS
    n <- length(sim.params)
    rndc <- do.call(paste0, replicate(5, sample(chars, n , TRUE), FALSE)) # generate unique ids
    sim_ids <- paste0(groupname,'_',rndc)  ## array of sim ids
    simheaders <- paste0(simheadbeg,sim_ids,simheadend) ## array of headers

    ## JOIN ALL SIM INFO
    simxml <- paste(simheaders,sim.params,outputs,'</Simulation>\n', sep="\n")

    ### SPLIT INTO CHUNKS
    chunks <- split (simxml, ceiling(seq_along(simxml)/chunksize))

    my_filenames <- paste0("xmls/",groupname,"_chunk_",1:length(chunks),".xml")
    ### WRITING
    w <- function (str) {
        write(str,file=my_filename,append=T)
    }

    for (i in 1:length(chunks)) {
        my_filename <- my_filenames[i]
        simxml <- chunks[[i]]

        file.create(my_filename)
        write(header, my_filename, append=FALSE)
        sapply(simxml,FUN=w)
        w(footer)
    }

    return(my_filenames)
}

## RUNNING

run_simulations <- function(my_filenames, outputdir) {
    for (filename in my_filenames) {
        system(paste0('gama-headless.bat ',filename,' ',outputdir))
    }
}
