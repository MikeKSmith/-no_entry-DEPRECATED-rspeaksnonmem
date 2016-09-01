#' Finds the appropriate command line for execution
#'
#' @param tool Name of tool e.g. NONMEM, VPC, bootstrap, SSE etc.
#' @param installInfo A named list containing path and command line string / shortcut.
#' List names should match software names.
#' @return path to the executable (.bat file on Windows or command on other platforms)
#' @details defineExecutable can help the user find the correct command line for use with `system( )`.
#' rspeaksnonmem contains a named list called installedSoftware that includes installation path and
#' command line for commonly used software (NONMEM, PsN commands).
#'
#' The user can specify the command line explicitly in each function e.g. `execute_PsN(command='c:/perl516/bin/execute.bat',...)`
#' but defineExecutable provides an automated means to construct that command line.
#' @note On the Windows platform, defineExecutable assumes that the necessary file extension is .bat.
#' On other platforms, defineExecutable uses `Sys.which( )` to determine the full path to any alias.
#' @examples
#' defineExecutable(tool='NONMEM', installInfo=installedSoftware)
#' defineExecutable(command='execute', searchCommand=F)
#' defineExecutable(installPath="c:/nm72/", command="nmfe72", searchCommand=T)
#' do.call(system,args=list(command=defineExecutable(command="execute-3.5.4")))

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
