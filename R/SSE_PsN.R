#' Performs PsN SSE for a given control file and dataset
#'
#' @param tool PsN tool. Must be used in conjunction with installInfo data.frame.
#' See Vignette "using rspeaksnonmem to run NONMEM and PsN".
#' @param command PsN command to be executed at the command line
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param samples Number of samples for SSE
#' @param psnOpts List of additional PsN command line arguments 
#' (format: argumentName = value or argumentName=TRUE/FALSE )
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @param clean Whether to clean up additional NONMEM files and folders 
#' following PsN call.  PsN option. Default = 1.
#' @return PsN SSE output
#' @examples
#'
#'## ----SSE:
SSE_PsN <- function(command = NULL, 
                    tool = "sse",
                    installPath = NULL,
                    version = NULL,
                    modelFile = NULL, 
                    samples = 100, 
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
  
  psnOpts <- c(list(samples = samples),
               psnOpts)

  callPsN(command = command,
          tool = "sse", 
          installPath = installPath,
          version = version,
          file = modelFile,
          psnOpts = psnOpts)  
}
