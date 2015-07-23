#' Estimates parameters using NONMEM
#'
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param reportExtension NONMEM control stream output file extension. Defaults to '.lst'
#' @param working.dir Working directory containing control stream and where output files should be stored
#' @param cleanup Whether to clean up additional NONMEM files and folders following estimation. Defaults to TRUE.
#' @param NMcommand Command line for calling NONMEM. For Windows must point to a file of type .bat or .exe.
#' @return NONMEM estimation output files
#' @examples
#' estimate.NM(modelFile='warfarin_PK_CONC_MKS', modelExtension='.ctl', working.dir='./data')

estimate.NM <- function(command = NULL,
                        modelFile = NULL, modelExtension = ".mod",
                        reportExtension = ".lst",
                        cleanup = T, working.dir = NULL, ... ) {
  orig.dir <- getwd()
  working.dir <- ifelse( is.null( working.dir ), getwd(), working.dir )
  setwd( working.dir )

  baseCommand <- ifelse(is.null(command), findExecutable("execute"), command)
  command <- paste(baseCommand, paste(modelFile, modelExtension, sep = ""), paste(modelFile, reportExtension, sep = ""))
  cat( paste(command, "\n") )
  execute( command )

  if (cleanup) cleanup()
  setwd(orig.dir)
}
