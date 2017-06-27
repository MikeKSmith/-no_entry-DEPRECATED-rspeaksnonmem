#' Performs VPC of a model using PsN VPC
#'
#' @param command Explicit PsN command to be run at shell/DOS prompt.
#' @param tool PsN tool name. To be used in conjunction with \code{"installPath"}
#'  and \code{"version"}. Defaults to "vpc".
#' @param installPath Installation path for Perl / PsN. e.g. "c:/strawberry/perl"
#' @param version Version of PsN as a character string. e.g. "4.6.0"
#' @param samples Number of samples for VPC. Mandatory option for PsN VPC.
#' Default = 100.
#' @param psnOpts List of additional PsN command line options 
#' (format: optionName = value or optionName=TRUE/FALSE )
#' @param modelFile NONMEM control stream file name (without extension)
#' @param clean Whether to clean up additional NONMEM files and folders 
#' following estimation.  PsN option. Default = 1.
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @return VPC output files
#' @examples
#' VPC_PsN(modelFile='warfarin_PK_CONC_MKS.ctl', working.dir='./data')
#' @export

VPC_PsN <- function(command = NULL, 
                    tool = "vpc",
                    installPath = NULL,
                    version = NULL,
                    samples = 100, 
                    psnOpts = NULL,
                    modelFile = NULL,
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
  
  psnOpts <- c(list(samples = samples, dir = working.dir, clean = clean), 
               psnOpts)
  
  baseCommand <- ifelse(is.null(command), 
                        defineExecutable(tool = "vpc",
                                         installPath = installPath,
                                         version = version), 
                        defineExecutable(command = command))
  
  callPsN(command = command,
          tool = "vpc", 
          installPath = installPath,
          version = version,
          file = modelFile,
          psnOpts = psnOpts)  
}


