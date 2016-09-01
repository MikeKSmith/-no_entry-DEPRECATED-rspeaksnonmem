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
SSE_PsN <- function(command = NULL, modelFile = NULL, modelExtension = ".mod", nsamp = 100, 
                    seed = 123456, addargs = NULL, cleanup = T, working.dir = NULL, ...) {
  
  working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)
  
  baseCommand <- ifelse( is.null( command ), 
                         defineExecutable( tool = "SSE" , ... ) , 
                         defineExecutable( command=command, ... ))
  
  command <- paste(baseCommand, shQuote(modelFile), " --samples=", 
                   nsamp, " --seed=", seed, " ", 
                   " --directory=", shQuote(working.dir),
                   if(cleanup) " --clean=2",
                   addargs, sep = "")
  cat(paste(command, "\n"))
  execute(command)
} 
