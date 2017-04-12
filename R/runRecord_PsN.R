#' Uses PsN RunRecord function to create a Run Record
#'
#' @param command Explicit PsN command to be run at shell/DOS prompt.
#' @param tool PsN tool name. To be used in conjunction with \code{"installPath"}
#'  and \code{"version"}. Defaults to "runrecord".
#' @param installPath Installation path for Perl / PsN. e.g. "c:/strawberry/perl"
#' @param version Version of PsN as a character string. e.g. "4.6.0"
#' @param to Run numbers to include in the Run Record.
#' @param runRoot Root for Run control and output files e.g. Run1.mod, Run1.lst 
#' would have runRoot='Run'
#' @param modelExtension Extension for NONMEM control stream files. 
#' Default is '.mod'.
#' @param outputExtension Extension for NONMEM output files. Default is '.lst'.
#' @param psnOpts Additional arguments for PsN runrecord function.
#' @param clean Clean up working directory following completion. PsN option.
#' Default = 1.
#' @param working.dir Working directory containing control streams and where 
#' output files should be stored
#' @return PsN Run Record output
#' @examples
#' @export
#' 
runRecord_PsN <- function(command = NULL, 
                          tool = "runrecord",
                          installPath = NULL,
                          version = NULL,
                          to = NULL, 
                          runRoot = "Run", 
                          modelExtension = ".mod",
                          outputExtension = ".lst", 
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
  
  psnOpts <- c(list(to=to),
               psnOpts) 
  
  psnOpts <- c(list(mod_ext = modelExtension),
               psnOpts)
  
  if (is.null(command)) {
    if (is.null(installPath)) stop("Please specify an installPath for PsN")
    if (is.null(version)) stop("Please specify a version of PsN")   
  }
  
  psnOptsText <- ifelse(!is.null(psnOpts), 
                        validate_PsN_options(tool = tool,
                                             installPath = installPath,
                                             version = version,
                                             psnOpts = psnOpts),
                        "")
  
  baseCommand <- ifelse(is.null(command), 
                        defineExecutable(tool = "runrecord",
                                         installPath = installPath,
                                         version = version),
                        defineExecutable(command = command))
  
  command <- paste(baseCommand, " ", 
                   psnOptsText)
  
  cat(paste(command, "\n"))
  execute(command = command)
  
  runRecord <- read.table("AAruninfo.txt", sep = ";", row.names = NULL, skip = 5, 
                          header = F, stringsAsFactors = F, as.is = T)
  
  names(runRecord) <- scan("AAruninfo.txt", sep = ";", what = "character", skip = 4, 
                           nlines = 1)
  
  runRecord$Run <- seq(1, to)
  runRecord <- runRecord[, -ncol(runRecord)]
  return(runRecord)
}
