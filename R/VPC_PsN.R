#' Performs VPC of a model using PsN VPC
#'
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param addargs Additional PsN command line arguments (text string)
#' @param working.dir Working directory containing control stream and where output files should be stored
#' @param cleanup Whether to clean up additional NONMEM files and folders following estimation. Defaults to TRUE.
#' @return NONMEM estimation output files
#' @examples
#' execute.PsN(modelFile='warfarin_PK_CONC_MKS', modelExtension='.ctl', working.dir='./data')
#'
VPC.PsN <- function(modelExtension = ".mod", modelFile, nsamp, seed, addargs, cleanup = T, working.dir = NULL, ...) {
    orig.dir <- getwd()
    if (is.null(working.dir)) 
        working.dir <- getwd() else setwd(working.dir)
    
    if (.Platform$OS.type == "windows") {
        command <- "c:\\pkpd\\bin\\vpc-3.5.4.bat "
        command <- paste(command, shQuote(paste(modelFile, modelExtension, sep = "")), " --samples=", nsamp, " --seed=", seed, " ", addargs, sep = "")
        cat(paste(command, "\n"))
        args <- list(command)
        do.call(system, args)
    }
    if (.Platform$OS.type != "windows") {
        command <- "vpc-4.2.0 "
        command <- paste(command, shQuote(paste(modelFile, modelExtension, sep = "")), " --samples=", nsamp, " --seed=", seed, " ", addargs, sep = "")
        cat(paste(command, "\n"))
        system(command)
    }
    
    if (cleanup) 
        cleanup()
    setwd(orig.dir)
} 
