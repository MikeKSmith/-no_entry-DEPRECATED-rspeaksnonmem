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
#' The user can specify the command line explicitly in each function e.g. `execute_PsN(command='c:/perl516/bin/execute.bat',...)`
#' but findExecutable provides an automated means to construct that command line.
#' @note On the Windows platform, findExecutable assumes that the necessary file extension is .bat.
#' On other platforms, findExecutable uses `Sys.which( )` to determine the full path to any alias.
#' @examples
#' findExecutable(software='NONMEM')
#' findExecutable(software='execute')
#' findExecutable(software='VPC')
#' do.call(system,args=list(command=findExecutable(software='execute')))

installedSoftware <- list(NONMEM = list(path = "c:/SEE/nm_7.3.0_g", command = "nmfe73"), execute = list(path = "c:/SEE/perl/", 
    command = "execute-4.4.8"), VPC = list(path = "c:/SEE/perl/", command = "vpc-4.4.8"), bootstrap = list(path = "c:/SEE/perl/", 
    command = "bootstrap-4.4.8"), SSE = list(path = "c:/SEE/perl/", command = "sse-4.4.8"), runrecord = list(path = "c:/SEE/perl", 
    command = "runrecord-4.4.8"))

findExecutable <- function(software = NULL, installInfo = installedSoftware) {
    whichSoftware <- names(installInfo[casefold(names(installInfo), upper = T) == casefold(software, 
        upper = T)])
    mySoftware <- installInfo[[whichSoftware]]
    
    if (win()) {
        if (casefold(software, upper = T) == "NONMEM") {
            mySoftwareSearchPath <- file.path(mySoftware$path, "run")
        }
        if (casefold(software, upper = T) != "NONMEM") {
            mySoftwareSearchPath <- file.path(mySoftware$path, "bin")
        }
        return(paste(file.path(mySoftwareSearchPath, mySoftware$command), ".bat", sep = ""))
    }
    if (!win()) {
        return(Sys.which(mySoftware$command))
    }
} 
