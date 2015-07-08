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

estimate.NM <- function(modelFile = NULL, modelExtension = ".mod", 
                        reportExtension = ".lst", working.dir = NULL, 
                        cleanup = T, 
                        NMcommand = if(win()){"c:\\pkpd\\bin\\nonmem-7.2.bat"} 
                        else {"c:\\pkpd\\bin\\nonmem-7.2.bat"}) {
  orig.dir <- getwd()
  if (is.null(working.dir)) 
    working.dir <- getwd() else setwd(working.dir)
  
  command <- NMcommand
  command <- paste(command, paste(modelFile, modelExtension, sep = ""), paste(modelFile, reportExtension, sep = ""))
  cat(paste(command, "\n"))
  execute(command)
  
  if (cleanup) 
    cleanup()
  setwd(orig.dir)
} 
