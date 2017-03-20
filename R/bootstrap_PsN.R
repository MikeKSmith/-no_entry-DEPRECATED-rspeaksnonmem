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
bootstrap_PsN <- function(command = NULL, 
                          tool = "bootstrap",
                          installPath = NULL,
                          version = NULL,
                          modelFile = NULL, 
                          psnOpts = NULL, 
                          clean = 1, 
                          working.dir = NULL) {
  
  if (!is.null(working.dir)) {
    psnOpts <- c(list(directory = working.dir),
                 psnOpts)
  }
  
  if (clean != 1) {
    psnOpts <- c(list(clean = clean),
                 psnOpts)
  }
  
  callPsN(command = command,
          tool = "bootstrap", 
          installPath = installPath,
          version = version,
          file = modelFile,
          psnOpts = psnOpts)
  }

