#' utility functions used within the package
#'
#'
win <- function() {
    .Platform$OS.type == "windows"
}

## -- from metrumrg runCommand.R --------------------------------------------- set up the call
execute <- function(command, intern = FALSE, minimized = FALSE, invisible = TRUE) {
    args <- list(command = command, intern = intern)
    if (win()) 
        args <- c(args, list(minimized = minimized, invisible = invisible))
    # cat(command, file=file.path(rdir,glue(run,'.cat')),sep='\n',append=TRUE)
    result <- tryCatch(do.call(system, args), error = function(e) warning(e$message, call. = FALSE, immediate. = TRUE))
    # if (is.integer(result)) result <- paste('Run',run,'has exit code',result) cat(result,file=file.path(rdir,glue(run,'.cat')),sep='\n',append=TRUE)
    return(result)  #visible
}

#' ---cleanup: function to remove NONMEM cruft.
#' Based on Andy Hooker's cleanup.R function https://github.com/andrewhooker/MBAOD/blob/master/R/cleanup.R
cleanup <- function(working.dir = NULL, pattern = NULL, remove.folders = F, ...) {
    
    orig.dir <- getwd()
    if (is.null(working.dir)) 
        working.dir <- getwd() else setwd(working.dir)
    
    print("- Cleaning up..")
    
    # remove old files before new run
    files <- c("xml", "_L", "_R", "INTER", "LINK", "nul", "nmprd4p.mod", "nonmem", "FCON", "FDATA", "FMSG", "fort.6", "FREPORT", "FSIZES", "FSTREAM", 
        "FSUBS", "fsubs.f90", "fsubs.o", "FSUBS.MU.F90", "GFCOMPILE.BAT", "linkc", "nmfe72", "set", "newline", "gfortran", "prsizes", "trash", "compile", 
        "matlab", "garbage.out")
    if (length(pattern) > 0) 
        files <- c(files, pattern)
    
    foo <- sapply(files, function(x) unlink(dir(pattern = x)))
    unlink(paste(getwd(), "temp_dir", sep = "/"), recursive = T)
    
    # remove PsN folders
    if (remove.folders) {
        all <- list.files(all.files = F, full.names = T)
        alldirs <- all[file.info(all)$isdir]
        matchdirs <- alldirs
        if (length(pattern) > 0) 
            matchdirs <- alldirs[grep(pattern, alldirs)]
        unlink(matchdirs, recursive = T, force = T)
    }
    
    setwd(orig.dir)
} 
