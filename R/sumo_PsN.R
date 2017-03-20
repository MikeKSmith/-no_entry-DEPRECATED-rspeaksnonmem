#' Use PsN to summarise NONMEM output
#'
#' @param command PsN command.
#' @param tool PsN tool.
#' @param installPath Installation path for Perl / PsN
#' @param version Version of PsN
#' @param lstFile NONMEM output file name
#' @param psnOpts List of additional PsN command line arguments 
#' (format: argumentName = value or argumentName=TRUE/FALSE )
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @details Either specify an explicit command to run at the system prompt 
#' or specify a combination of tool, installPath and version.
#' @return PsN sumo output
#' @examples
#' sumo_PsN(modelFile='warfarin_PK_CONC_MKS.ctl', working.dir='./data')
#' @export

sumo_PsN <- function(command = NULL, 
                     tool = "sumo",
                     installPath = NULL,
                     version = NULL,
                     lstFile = NULL, 
                     psnOpts = NULL, 
                     working.dir = NULL) {
  
  if (!is.null(working.dir)) {
  psnOpts <- c(list(directory = working.dir),
               psnOpts)
  }
  
  callPsN(command = command,
          tool = "sumo", 
          installPath = installPath,
          version = version,
          file = lstFile,
          psnOpts = psnOpts)
  }
