#' Performs VPC of a model using PsN VPC
#'
#' @param tool PsN tool. Must be used in conjunction with installInfo data.frame.
#' See Vignette "using rspeaksnonmem to run NONMEM and PsN".
#' @param command PsN command to be executed at the command line
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to 
#' '.mod'
#' @param samples Number of samples for VPC. Mandatory option for PsN VPC. 
#' @param psnOpts List of additional PsN command line options 
#' (format: optionName = value or optionName=TRUE/FALSE )
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @param clean Whether to clean up additional NONMEM files and folders 
#' following estimation.  PsN option. Default = 1.
#' @return NONMEM estimation output files
#' @examples
#' VPC_PsN(modelFile='warfarin_PK_CONC_MKS.ctl', working.dir='./data')
#' @export

VPC_PsN <- function(command = NULL, 
                    tool = "sse",
                    installPath = NULL,
                    version = NULL,
                    samples = 100, 
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
  
  psnOpts <- c(list(samples = samples, dir = working.dir, clean = clean), 
               psnOpts)
  
  baseCommand <- ifelse(is.null(command), 
                        defineExecutable(tool = "VPC", ...), 
                        defineExecutable(command = command))
  
  callPsN(command = command,
          tool = "vpc", 
          installPath = installPath,
          version = version,
          file = modelFile,
          psnOpts = psnOpts)  
}


