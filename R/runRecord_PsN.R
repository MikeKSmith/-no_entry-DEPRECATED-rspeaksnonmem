#' Uses PsN RunRecord function to create a Run Record
#'
#' @param to Run numbers to include in the Run Record.
#' @param runRoot Root for Run control and output files e.g. Run1.mod, Run1.lst would have runRoot='Run'
#' @param modelExtension Extension for NONMEM control stream files. Default is '.mod'.
#' @param outputExtension Extension for NONMEM output files. Default is '.lst'.
#' @param addargs Additional arguments for PsN runrecord function.
#' @param cleanup Clean up directory once processing is finished?. Default TRUE.
#' @param working.dir Working directory containing control streams and where output files should be stored
#' @return PsN Run Record output
#' @examples
runRecord.PsN <- function(command=NULL, to = NULL, runRoot = "Run",
                          modelExtension = ".mod", outputExtension = ".lst",
                          addargs = NULL,
                          cleanup = T, working.dir = NULL, ...) {
  orig.dir <- getwd()
  working.dir <- ifelse( is.null( working.dir ), getwd(), working.dir )
  setwd( working.dir )

  modelFiles <- list.files(pattern = paste(runRoot, "[0-9]\\", modelExtension, sep = ""), recursive = T)
  lstFiles <- list.files(pattern = paste(runRoot, "[0-9]\\", outputExtension, sep = ""), recursive = T)

  runpath <- file.path(getwd(), "RunRecord")
  dir.create(runpath)
  file.copy(modelFiles, file.path(runpath))
  file.copy(lstFiles, file.path(runpath))
  setwd(runpath)

  baseCommand <- ifelse(is.null(command), findExecutable("runrecord"), command)
  command <- paste(baseCommand, " --to=", to, " ", addargs, sep = "")
  cat(paste(command, "\n"))
  system(command)

  runRecord <- read.table("AAruninfo.txt", sep = ";", row.names = NULL, skip = 5, header = F, stringsAsFactors = F, as.is = T)
  names(runRecord) <- scan("AAruninfo.txt", sep = ";", what = "character", skip = 4, nlines = 1)
  runRecord$Run <- seq(1, to)
  runRecord <- runRecord[, -ncol(runRecord)]
  if (cleanup) cleanup()
  return(runRecord)
  setwd(orig.dir)
}
