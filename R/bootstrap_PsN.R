#' Performs PsN bootstrap for a given control file and dataset
#'
#' @param tool PsN tool. Must be used in conjunction with installInfo data.frame.
#' See Vignette "using rspeaksnonmem to run NONMEM and PsN".
#' @param command PsN command to be executed at the command line
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param psnOpts List of additional PsN command line arguments 
#' (format: argumentName = value or argumentName=TRUE/FALSE )
#' @param clean Clean up working directory following completion. PsN option.
#' Default = 1.
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @return PsN SSE output
#' @examples
bootstrap_PsN <- function(tool = NULL, command = NULL, 
                          modelFile = NULL, modelExtension = ".mod", 
                          psnOpts = NULL, clean = 1, working.dir = NULL, 
                          ...) {
  
  working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)
  
  psnOpts <- c(list(directory = working.dir, clean = clean), 
               psnOpts)
  
  baseCommand <- ifelse(is.null(command), 
                        defineExecutable(tool = "bootstrap", ...), 
                        defineExecutable(command = command, ...))
  
  callPsN(baseCommand = baseCommand, modelFile = modelFile, 
          psnOpts = psnOpts)
  }

