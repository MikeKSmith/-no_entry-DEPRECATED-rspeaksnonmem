#' Finds the appropriate command line for execution
#'
#' @param tool Name of tool e.g. NONMEM, VPC, bootstrap, SSE etc.
#' @param command Command which will be executed at a shell prompt 
#' e.g. execute-3.5.4 or nmfe72.bat.
#' Use this instead of tool + installInfo to specify precisely what command to 
#' run at the shell prompt.
#' @param installPath Path to the installation of NONMEM or PsN
#' @param searchCommand (Boolean) Whether to search for .bat or .exe files 
#' matching the command. 
#' @return path to the executable (.bat file on Windows or command on other 
#' platforms)
#' @details defineExecutable can help the user find the correct command line for
#' use with `system( )`.
#' rspeaksnonmem contains a named list called installedSoftware that includes 
#' installation path and command line for commonly used software (NONMEM, 
#' PsN commands).
#'
#' The user can specify the command line explicitly in each function 
#' e.g. `execute_PsN(command='c:/perl516/bin/execute.bat',...)`
#' but defineExecutable provides an automated means to construct that command 
#' line.
#' @note * Using the 'command' option instead of specifying 'tool' and 
#' 'installInfo' will perform a check
#' of whether the command is valid using the R function Sys.which.
#' * Using the searchCommand option may return more than one executable. It is 
#' recommended to run defineExecutable with searchCommand = TRUE to determine 
#' which executable is suitable for use 
#' BEFORE using this option in estimate_NM or PsN execution functions.
#' @examples
#' defineExecutable(tool='NONMEM', installInfo=installedSoftware)
#' defineExecutable(command='execute', searchCommand=F)
#' defineExecutable(installPath='c:/nm72/', command='nmfe72', searchCommand=T)
#' do.call(system,args=list(command=defineExecutable(command='execute-3.5.4')))
#' @export

defineExecutable <- function(tool = NULL, 
                             version = NULL,
                             command = NULL, 
                             installPath = NULL, 
                             searchCommand = FALSE, ...) {
  
  if(is.null(tool) && is.null(command)) stop("One of tool or command must be specified")
  
  ## If the user wants to search for possible executables then one of command
  ## or tool + installPath must be given.
  if (searchCommand){
    # If the user supplies the command with no file path with searchCommand=T 
    # then use Sys.which or Sys.which2 (depending on OS) to search PATH for 
    # executables.
    if(!is.null(command) && dirname(command)==".") {
      command <- ifelse(win(), Sys.which2(command), Sys.which(command))
    } 
    # If the user supplies command with full file path then search within the
    # file path for the command.
    if(!is.null(command) && dirname(command)!=".") {
      allMatches <- dir(dirname(command),basename(command),recursive = T,full.names = T)
      isValidExecutable <- check.executable(allMatches)
      command <- allMatches[isValidExecutable]
    }
    # If the user does not supply a command, but supplies tool + installPath
    # then search within the installPath for matching executables.
    if(is.null(command) && !is.null(tool) && !is.null(installPath)) {
      allMatches <- dir(installPath, tool, recursive = T, full.names=T)
      isValidExecutable <- check.executable(allMatches)
      command <- allMatches[isValidExecutable]
    }
    if (length(command)==0) stop("No matches found for the provided command or tool")
  } else {
    ## If the user provides the command and is NOT using searchCommand then 
    ## we assume that they have given a valid command including the full file path
    ## to the executable OR they have a shortcut defined via the PATH variables.
    if (!is.null(command)) command <- command
    
    ## if the user has specified a tool they must also specify the installPath
    if (is.null(command) && !is.null(tool)){
      if (is.null(installPath)) stop("If using tool, please specify an installPath")
      if (is.null(version)) stop("If using tool, please specify a version")      
      
      # Combine installPath, tool and version to infer likely command.
      # IF NONMEM then paste installPath, "run","nmfe" version without "." and ".bat"
      if (casefold(tool, upper = T) == "NONMEM"){
        if (is.null(version)){
          stop("Require a version number for NONMEM to create nmfe executable")
        } else {
          command <- file.path(installPath,
                               "run",
                               paste("nmfe",
                                     gsub("\\.","",version),
                                     ".bat", 
                                     sep="")
          )
        }
      } else {
        # If the user specifies a version number then assume that they want to 
        # call the version identified perl script within installPath/bin.
        # We make the assumption that they will also want to use the perl version
        # within the installPath/bin. 
        # Using the versioned .bat file would make assumptions about Perl being
        # on the PATH and that this version matches the version to be used with
        # the perl script.
        if(!is.null(version)){
          command <- paste(
            file.path(installPath,
                      "bin",
                      "perl"),
            file.path(installPath,
                      "bin",
                      paste(tool,
                            "-",
                            version,
                            ".pl",
                            sep="")
            )
          )
        } else {
          # If PsN and no version given then we assume the user will want to call
          # the .bat scripts within the Perl installation.
          command <- file.path(installPath,
                               "bin",
                               paste(tool,
                                     ".bat",
                                     sep="")
          )
        }
      }
    }
  }
  return(command)
}
