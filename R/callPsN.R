#' callPsN
#'
#' @param command PsN command.
#' @param tool PsN tool.
#' @param installPath Installation path for Perl / PsN
#' @param version Version of PsN
#' @param file File for action with PsN. Usually the model file.
#' For sumo it can be the lstFile.
#' @param psnOpts Additional PsN command line arguments (text string)
#' @return Validate the arguments and execute the PsN command 
#'
callPsN <- function(command = NULL, 
                    tool = NULL,
                    installPath = NULL,
                    version = NULL,
                    file = NULL, 
                    psnOpts = NULL) {
  
  ## if the user has specified a tool they must also specify the installPath
  if (is.null(command)) {
    if (is.null(installPath)) stop("Please specify an installPath for PsN")
    if (is.null(version)) stop("Please specify a version of PsN")   
  }

  if (!file.exists(file)) stop(paste("File",file,"cannot be found"))
    
  psnOptsText <- ifelse(!is.null(psnOpts), 
                        validate_PsN_options(tool = tool,
                                             installPath = installPath,
                                             version = version,
                                             psnOpts = psnOpts),
                        "")
  
  baseCommand <- ifelse(is.null(command), 
                        defineExecutable(tool = tool,
                                         installPath = installPath,
                                         version = version),
                        defineExecutable(command = command))
  
  command <- paste(baseCommand, " ", 
                   shQuote(file), 
                   psnOptsText)
  
  cat(paste(command, "\n"))
  execute(command = command)
}
