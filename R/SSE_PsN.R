#' Performs PsN SSE for a given control file and dataset
#'
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param addargs Additional PsN command line arguments (text string)
#' @param working.dir Working directory containing control stream and where output files should be stored
#' @return PsN SSE output
#' @examples
#'
#'## ----SSE:
SSE.PsN <- function(modelFile, modelExtension = ".mod", nsamp, seed, addargs = NULL, cleanup = T, working.dir = NULL, ...) {
    orig.dir <- getwd()
    if (is.null(working.dir)) 
        working.dir <- getwd() else setwd(working.dir)
    
    if (.Platform$OS.type == "windows") {
        command <- "c:\\pkpd\\bin\\sse-3.5.4.bat "
        command <- paste(command, shQuote(paste(modelFile, modelExtension, sep = "")), " --samples=", nsamp, " --seed=", seed, " ", addargs, sep = "")
        cat(paste(command, "\n"))
        args <- list(command)
        do.call(system, args)
    }
    if (.Platform$OS.type != "windows") {
        command <- "sse-4.2.0 "
        command <- paste(command, shQuote(paste(modelFile, modelExtension, sep = "")), " --samples=", nsamp, " --seed=", seed, " ", addargs, sep = "")
        cat(paste(command, "\n"))
        system(command)
    }
    
    if (cleanup) 
        cleanup()
    setwd(orig.dir)
} 
