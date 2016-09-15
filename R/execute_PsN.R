#' Estimates parameters using PsN execute
#'
#' @param tool PsN tool. Must be used in conjunction with installInfo data.frame.
#' See Vignette "using rspeaksnonmem to run NONMEM and PsN".
#' @param command PsN command to be executed at the command line
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param addargs Additional PsN command line arguments (text string)
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @param cleanup Whether to clean up additional NONMEM files and folders 
#' following estimation. Defaults to TRUE.
#' @return NONMEM estimation output files
#' @examples
#' execute_PsN(modelFile='warfarin_PK_CONC_MKS.ctl', working.dir='./data')
#'
execute_PsN <- function(tool = NULL, command = NULL, 
                        modelFile = NULL, psnOpts = NULL, 
                        clean = 1, working.dir = NULL, ...) {
  
  working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)
  
  psnOpts <- c(list(clean = clean, dir = working.dir),
               psnOpts)
  
  baseCommand <- ifelse(is.null(command), 
                        defineExecutable(tool = "execute", ...),
                        defineExecutable(command = command, ...))
  
  callPsN(baseCommand = baseCommand, modelFile = modelFile, 
          psnOpts = psnOpts)
}
