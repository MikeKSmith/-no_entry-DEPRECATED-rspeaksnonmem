#' callPsN
#'
#' @param command Explicit PsN command to be run at shell/DOS prompt.
#' @param tool PsN tool name. To be used in conjunction with \code{"installPath"}
#'  and \code{"version"}.
#' @param installPath Installation path for Perl / PsN. e.g. "c:/strawberry/perl"
#' @param version Version of PsN as a character string. e.g. "4.6.0"
#' @param file File for action with PsN. Usually the model file.
#' For sumo it can be the lstFile.
#' @param psnOpts Additional PsN command line arguments in list format. 
#' (format: optionName = value or optionName=TRUE/FALSE )
#' @details The user has two options: to provide the explicit command for 
#' execution at the command or shell prompt OR to provide the tool name, 
#' installation path of Perl and version of PsN. 
#' callPsN checks option names and value types against expected options for the 
#' PsN tool before execution.
#' @return Validate the options and execute the PsN command 
#' @export

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
