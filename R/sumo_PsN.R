#' Use PsN to summarise NONMEM output
#'
#' @param tool PsN tool. Must be used in conjunction with installInfo data.frame.
#' See Vignette "using rspeaksnonmem to run NONMEM and PsN".
#' @param command PsN command to be executed at the command line
#' @param lstFile NONMEM output file name
#' @param psnOpts List of additional PsN command line arguments 
#' (format: argumentName = value or argumentName=TRUE/FALSE )
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @return PsN sumo output
#' @examples
#' execute_PsN(modelFile='warfarin_PK_CONC_MKS.ctl', working.dir='./data')
#' @export

sumo_PsN <- function(tool = NULL, command = NULL, 
                     lstFile = NULL, psnOpts = NULL, 
                     working.dir = NULL, ...) {
  
  working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)
  
  psnOpts <- c(list(dir = working.dir),
               psnOpts)
  
  baseCommand <- ifelse(is.null(command), 
                        defineExecutable(tool = "sumo", ...), 
                        defineExecutable(command = command, ...))
  
  callPsN(baseCommand = baseCommand, modelFile = lstFile, 
          psnOpts = psnOpts)
  }
