#' Estimates parameters using PsN execute
#'
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param addargs Additional PsN command line arguments (text string)
#' @param working.dir Working directory containing control stream and where output files should be stored
#' @param cleanup Whether to clean up additional NONMEM files and folders following estimation. Defaults to TRUE.
#' @return NONMEM estimation output files
#' @examples
#' execute_PsN(modelFile='warfarin_PK_CONC_MKS', modelExtension='.ctl', working.dir='./data')
#'
execute_PsN <- function(command = NULL, modelFile = NULL, modelExtension = ".mod", addargs = NULL, 
    cleanup = T, working.dir = NULL, ...) {
    working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)

    baseCommand <- ifelse(is.null(command), findExecutable("execute"), command)
    command <- paste(baseCommand, paste(modelFile, modelExtension, sep = ""), 
                     " --directory=", working.dir,
                     if(cleanup) " --clean=2",
                     addargs)
    cat(paste(command, "\n"))
    do.call(system, args = list(command = command))
} 
