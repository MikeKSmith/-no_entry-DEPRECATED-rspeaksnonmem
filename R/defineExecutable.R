#' Finds the appropriate command line for execution
#'
#' @param tool Name of tool e.g. NONMEM, VPC, bootstrap, SSE etc.
#' @param installInfo A named list or data frame containing path and command line string / shortcut.
#' List names should match software names. Acts as a lookup table for specifying commands.
#' @param command Command which will be executed at a shell prompt e.g. execute-3.5.4 or nmfe72.bat.
#' Use this instead of tool + installInfo to specify precisely what command to run at the shell prompt.
#' @param installPath Path to the installation of NONMEM or PsN
#' @param searchCommand (Boolean) Whether to search for .bat or .exe files matching the command. 
#' @return path to the executable (.bat file on Windows or command on other platforms)
#' @details defineExecutable can help the user find the correct command line for use with `system( )`.
#' rspeaksnonmem contains a named list called installedSoftware that includes installation path and
#' command line for commonly used software (NONMEM, PsN commands).
#'
#' The user can specify the command line explicitly in each function e.g. `execute_PsN(command='c:/perl516/bin/execute.bat',...)`
#' but defineExecutable provides an automated means to construct that command line.
#' @note * Using the "command" option instead of specifying "tool" and "installInfo" will perform a check
#' of whether the command is valid using the R function Sys.which.
#' * Using the searchCommand option may return more than one executable. It is recommended to 
#' run defineExecutable with searchCommand = TRUE to determine which executable is suitable for use 
#' BEFORE using this option in estimate_NM or PsN execution functions.
#' @examples
#' defineExecutable(tool='NONMEM', installInfo=installedSoftware)
#' defineExecutable(command='execute', searchCommand=F)
#' defineExecutable(installPath="c:/nm72/", command="nmfe72", searchCommand=T)
#' do.call(system,args=list(command=defineExecutable(command="execute-3.5.4")))
#' @export

defineExecutable <- function( tool = NULL, installInfo = NULL, command=NULL, installPath=NULL, searchCommand=FALSE, ...) {
  
  ## TODO review metrumrg methods for calling NONMEM runNonmem
  
  if( is.null(installInfo) ) {
    if( !searchCommand ) {
      command <- ifelse( win() , Sys.which2( command ), Sys.which( command ) )
    }
    else {
      command <- dir( path = installPath, pattern = command, recursive=T, full.names=T )   
    }
    return(command)
  }
  
  if( !is.null( installInfo ) ) {
    whichSoftware <- casefold(installInfo$tool, upper = T) == casefold(tool, upper = T)
    mySoftware <- installInfo[whichSoftware,]
    
    if( !is.na( mySoftware$path ) )
      if (casefold(tool, upper = T) == "NONMEM") {
        if (mySoftware$path!="") {
          myPath <- file.path(mySoftware$path, "run")
          mySoftwareCall <- paste(file.path(myPath, mySoftware$command), ".bat", sep = "")
          return(mySoftwareCall)
        }
      }
    
    if (casefold(tool, upper = T) != "NONMEM") {
      myPath <- file.path(mySoftware$path, "bin")
      mySoftwareCall <- paste(file.path(myPath,"perl"), " ", file.path(myPath,mySoftware$command), ".pl", sep = "")
      return(mySoftwareCall)
    }
    
    if ( is.na( mySoftware$path ) ){
      return( mySoftware$command )
    }
  } 
}
