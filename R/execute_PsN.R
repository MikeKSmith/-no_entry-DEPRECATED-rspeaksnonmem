#' Estimates parameters using PsN execute
#'
#' @param command Explicit PsN command to be run at shell/DOS prompt.
#' @param tool PsN tool name. To be used in conjunction with \code{"installPath"}
#'  and \code{"version"}. Defaults to "execute".
#' @param installPath Installation path for Perl / PsN. e.g. "c:/strawberry/perl"
#' @param version Version of PsN as a character string. e.g. "4.6.0"
#' @param modelFile NONMEM control stream file name (without extension)
#' @param psnOpts List of additional PsN command line arguments 
#' (format: argumentName = value or argumentName=TRUE/FALSE )
#' @param clean Whether to clean up additional NONMEM files and folders 
#' following PsN call.  PsN option. Default = 1.
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @return NONMEM estimation output files
#' @examples
#' execute_PsN(modelFile='warfarin_PK_CONC_MKS.ctl', working.dir='./data')
#' @export
#' 
execute_PsN <- function(command = NULL, 
                        tool = "execute",
                        installPath = NULL,
                        version = NULL,
                        modelFile = NULL, 
                        psnOpts = NULL,
                        clean = 1, 
                        working.dir = NULL, ...) {

  if (!is.null(working.dir)) {
    psnOpts <- c(list(directory = working.dir),
                 psnOpts)
  }

  if (clean != 1) {
    psnOpts <- c(list(clean = clean),
                 psnOpts)
  }
  
  
  callPsN(command = command,
          tool = "execute", 
          installPath = installPath,
          version = version,
          file = modelFile,
          psnOpts = psnOpts)
}
