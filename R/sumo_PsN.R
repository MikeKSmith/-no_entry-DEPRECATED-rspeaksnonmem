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
  
  if (!is.null(working.dir)) {
  psnOpts <- c(list(directory = working.dir),
               psnOpts)
  }
  
  if (is.null(command) && is.null(installInfo))
    stop("command or installInfo argument must be provided")
  
  baseCommand <- ifelse(!is.null(command) | !is.null(installInfo), 
                        defineExecutable(tool = "sumo", installInfo=installInfo), 
                        defineExecutable(command = command))
  
  callPsN(baseCommand = baseCommand, modelFile = lstFile, 
          psnOpts = psnOpts)
  }
