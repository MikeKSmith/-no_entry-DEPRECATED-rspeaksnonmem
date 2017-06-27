#' Performs PsN SSE for a given control file and dataset
#'
#' @param command Explicit PsN command to be run at shell/DOS prompt.
#' @param tool PsN tool name. To be used in conjunction with \code{"installPath"}
#'  and \code{"version"}. Defaults to "sse".
#' @param installPath Installation path for Perl / PsN. e.g. "c:/strawberry/perl"
#' @param version Version of PsN as a character string. e.g. "4.6.0"
#' @param modelFile NONMEM control stream file name (without extension)
#' @param samples Number of samples for SSE. Required argument. Default = 100.
#' @param psnOpts List of additional PsN command line arguments 
#' (format: argumentName = value or argumentName=TRUE/FALSE )
#' @param clean Whether to clean up additional NONMEM files and folders 
#' following PsN call.  PsN option. Default = 1.
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @return PsN SSE output
#' @examples
#' @export
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
