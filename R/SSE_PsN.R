#' Performs PsN SSE for a given control file and dataset
#'
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param addargs List of additional PsN command line arguments (format: argumentName = value or argumentName=TRUE/FALSE )
#' @param working.dir Working directory containing control stream and where output files should be stored
#' @return PsN SSE output
#' @examples
#'
#'## ----SSE:
SSE_PsN <- function(command = NULL, modelFile = NULL, modelExtension = ".mod", nsamp = 100, 
                    seed = 123456, addargs = NULL, cleanup = T, working.dir = NULL, ...) {
  
  addargsText <- ifelse(!is.null(addargs),list_to_PsNArgs(addargs), "")
  
  working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)
  
  baseCommand <- ifelse( is.null( command ), 
                         defineExecutable( tool = "SSE" , ... ) , 
                         defineExecutable( command=command, ... ))
  
  command <- paste(baseCommand, shQuote(modelFile), " --samples=", 
                   nsamp, " --seed=", seed, " ", 
                   " --directory=", shQuote(working.dir),
                   if(cleanup) " --clean=2", " ",
                   addargsText, sep = "")
  cat(paste(command, "\n"))
  execute(command)
} 
