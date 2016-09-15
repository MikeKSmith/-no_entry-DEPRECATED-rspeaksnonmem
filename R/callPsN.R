#' callPsN
#'
#' @param command PsN command to be executed at the command line
#' @param modelFile File for action with PsN. Usually the model file.
#' For sumo it can be the lstFile.
#' @param psnOpts Additional PsN command line arguments (text string)
#' @return Validate the arguments and execute the PsN command 
#'
callPsN <- function(baseCommand = NULL, modelFile = NULL, 
                    psnOpts = NULL) {
  
  psnOptsText <- ifelse(!is.null(psnOpts), 
                        validate_PsN_options(command = baseCommand,
                                             psnOpts = psnOpts),
                        "")
  
  command <- paste(baseCommand, " ", 
                   shQuote(modelFile), 
                   psnOptsText)
  
  cat(paste(command, "\n"))
  execute(command = command)
}
