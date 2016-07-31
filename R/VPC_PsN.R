#' Performs VPC of a model using PsN VPC
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
VPC_PsN <- function(command = NULL, modelFile = NULL, modelExtension = ".mod", nsamp = 100, 
    seed = 123456, addargs = NULL, cleanup = T, working.dir = NULL, ...) {
    working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)

    baseCommand <- ifelse(is.null(command), findExecutable("VPC"), command)
    command <- paste(baseCommand, shQuote(paste(modelFile, modelExtension, sep = "")), " --samples=", 
        nsamp, " --seed=", seed, " ", 
        " --directory=", working.dir,
        if(cleanup) " --clean=2",
        addargs, sep = "")
    cat(paste(command, "\n"))
    system(command)
} 
