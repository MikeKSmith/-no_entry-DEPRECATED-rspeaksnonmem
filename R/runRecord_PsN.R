#' Uses PsN RunRecord function to create a Run Record
#'
#' @param to Run numbers to include in the Run Record.
#' @param runRoot Root for Run control and output files e.g. Run1.mod, Run1.lst 
#' would have runRoot='Run'
#' @param modelExtension Extension for NONMEM control stream files. 
#' Default is '.mod'.
#' @param outputExtension Extension for NONMEM output files. Default is '.lst'.
#' @param psnOpts Additional arguments for PsN runrecord function.
#' @param cleanup Clean up directory once processing is finished?. Default TRUE.
#' @param working.dir Working directory containing control streams and where 
#' output files should be stored
#' @return PsN Run Record output
#' @examples
#' 
runRecord_PsN <- function(tool = NULL, command = NULL, to = NULL, 
                          runRoot = "Run", modelExtension = ".mod",
                          outputExtension = ".lst", psnOpts = NULL, 
                          clean = 1, working.dir = NULL, ...) {
  
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
  
  modelFiles <- list.files(pattern = paste(runRoot, "[0-9]\\", modelExtension,
                                           sep = ""), 
                           recursive = T)
  lstFiles <- list.files(pattern = paste(runRoot, "[0-9]\\", outputExtension, 
                                         sep = ""),
                         recursive = T)
  
  runpath <- file.path(working.dir, "RunRecord")
  dir.create(runpath)
  
  file.copy(modelFiles, file.path(runpath))
  file.copy(lstFiles, file.path(runpath))
  
  baseCommand <- ifelse(is.null(command), 
                        defineExecutable(tool = "runRecord", ...), 
                        defineExecutable(command = command))
  
  callPsN(baseCommand = baseCommand, modelFile = modelFile, 
          psnOpts = psnOpts)
  
  runRecord <- read.table("AAruninfo.txt", sep = ";", row.names = NULL, skip = 5, 
                          header = F, stringsAsFactors = F, as.is = T)
  
  names(runRecord) <- scan("AAruninfo.txt", sep = ";", what = "character", skip = 4, 
                           nlines = 1)
  
  runRecord$Run <- seq(1, to)
  runRecord <- runRecord[, -ncol(runRecord)]
  if (cleanup) 
    cleanup()
  return(runRecord)
}
