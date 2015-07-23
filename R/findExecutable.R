#' Finds the appropriate command line for execution
#'
#' @param software Name of software e.g. NONMEM, VPC, bootstrap, SSE etc.
#' @param installInfo A named list containing path and command line string / shortcut.
#' List names should match software names.
#' @return path to the executable (.bat file on Windows or command on other platforms)
#' @details findExecutable can help the user find the correct command line for use with `system( )`.
#' rspeaksnonmem contains a named list called installedSoftware that includes installation path and
#' command line for commonly used software (NONMEM, PsN commands).
#'
#' The user can specify the command line explicitly in each function e.g. `execute_PsN(command="c:/perl516/bin/execute.bat",...)`
#' but findExecutable provides an automated means to construct that command line.
#' @note On the Windows platform, findExecutable assumes that the necessary file extension is .bat.
#' On other platforms, findExecutable uses `Sys.which( )` to determine the full path to any alias.
#' @examples
#' findExecutable(software="NONMEM")
#' findExecutable(software="execute")
#' findExecutable(software="VPC")
#' do.call(system,args=list(command=findExecutable(software="execute")))

installedSoftware <- list(
  NONMEM    = list(path="c:/nm72",    command="nmfe72"),
  execute   = list(path="c:/perl516", command="execute"),
  VPC       = list(path="c:/perl516", command="vpc"),
  bootstrap = list(path="c:/perl516", command="bootstrap"),
  SSE       = list(path="c:/perl516", command="sse"),
  runrecord = list(path="c:/perl516", command="runrecord")
)


findExecutable <- function(software = NULL, installInfo = installedSoftware){
  whichSoftware <-names(installInfo[casefold(names(installInfo),upper=T)==casefold(software,upper=T)])
  mySoftware <- installInfo[[whichSoftware]]

  if( win() ){
    if(casefold(software,upper=T) == "NONMEM"){
      mySoftwareSearchPath <- file.path(mySoftware$path,"run")
    }
    if(casefold(software,upper=T) != "NONMEM"){
      mySoftwareSearchPath <- file.path(mySoftware$path,"bin")
    }
    return(paste(file.path(mySoftwareSearchPath,mySoftware$command),".bat",sep=""))
  }
  if( !win() ){
    return(Sys.which(mySoftware$command))
  }
}
