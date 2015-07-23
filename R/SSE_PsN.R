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
SSE.PsN <- function(command=NULL,
                    modelFile = NULL, modelExtension = ".mod",
                    nsamp = 100, seed = 123456,
                    addargs = NULL,
                    cleanup = T, working.dir = NULL, ...) {
  orig.dir <- getwd()
  working.dir <- ifelse( is.null( working.dir ), getwd(), working.dir )
  setwd( working.dir )

  baseCommand <- ifelse(is.null(command), findExecutable("SSE"), command)
  command <- paste(baseCommand, shQuote(paste(modelFile, modelExtension, sep = "")), " --samples=", nsamp, " --seed=", seed, " ", addargs, sep = "")
  cat(paste(command, "\n"))
  system(command)

if (cleanup) cleanup()
setwd(orig.dir)
}
