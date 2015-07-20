#' Performs PsN bootstrap for a given control file and dataset
#'
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param nsamp Number of bootstrap samples
#' @param seed Seed number for bootstrap (to be passed to PsN)
#' @param addargs Additional PsN command line arguments (text string)
#' @param cleanup Clean up working directory following completion, leaving only summary files. Default TRUE.
#' @param working.dir Working directory containing control stream and where output files should be stored
#' @return PsN SSE output
#' @examples
bootstrap.PsN <- function(modelFile, modelExtension = ".mod", nsamp, seed, addargs = NULL, cleanup = T, working.dir = NULL, ...) {
    orig.dir <- getwd()
    working.dir <- ifelse( is.null( working.dir ), getwd(), working.dir )
    setwd( working.dir )
    
    if (win()) {
        command <- "c:\\pkpd\\bin\\bootstrap-3.5.4.bat "
        command <- paste(command, shQuote(paste(modelFile, modelExtension, sep = "")), " --samples=", nsamp, " --seed=", seed, " ", addargs, sep = "")
        cat(paste(command, "\n"))
        args <- list(command)
        do.call(system, args)
    }
    if (!win()) {
        command <- "bootstrap-4.2.0 "
        command <- paste(command, shQuote(paste(modelFile, modelExtension, sep = "")), " --samples=", nsamp, " --seed=", seed, " ", addargs, sep = "")
        cat(paste(command, "\n"))
        system(command)
    }
    
    if (cleanup) cleanup()
    setwd(orig.dir)
} 
