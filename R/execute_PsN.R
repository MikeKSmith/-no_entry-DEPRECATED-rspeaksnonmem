#' Estimates parameters using PsN execute
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
execute.PsN <- function(modelFile = NULL, modelExtension = ".mod", addargs = "", working.dir = NULL, cleanup = T, ...) {
    orig.dir <- getwd()
    working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)
    setwd(working.dir)
    
    if (win()) {
        command <- "c:\\pkpd\\bin\\execute-3.5.4.bat"
        
        command <- paste(command, paste(modelFile, modelExtension, sep = ""))
        cat(paste(command, "\n"))
        
        args <- list(command)
        do.call(system, args)
    }
    if (!win()) {
        command <- "execute-3.5.4"
        command <- paste(command, paste(modelFile, modelExtension, sep = ""), addargs)
        cat(paste(command, "\n"))
        system(command)
    }
    
    if (cleanup) 
        cleanup()
    
    setwd(orig.dir)
} 
