#' Finds the appropriate command line for execution
#'
#' @param software Name of software e.g. NONMEM, VPC, bootstrap, SSE etc.
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
#' defineExecutable(software='NONMEM')
#' defineExecutable(software='execute')
#' defineExecutable(software='VPC')
#' do.call(system,args=list(command=defineExecutable(software='execute')))

defineExecutable <- function(software = NULL, installInfo = installedSoftware) {
    whichSoftware <- names(installInfo[casefold(names(installInfo), upper = T) == casefold(software, 
        upper = T)])
    mySoftware <- installInfo[[whichSoftware]]
    
    if (win()) {
        if (casefold(software, upper = T) == "NONMEM") {
            myPath <- file.path(mySoftware$path, "run")
            mySoftwareCall <- paste(file.path(myPath, mySoftware$command), ".bat", sep = "")
        }
        if (casefold(software, upper = T) != "NONMEM") {
            myPath <- file.path(mySoftware$path, "bin")
            mySoftwareCall <- paste(file.path(myPath,"perl"), " ", file.path(myPath,mySoftware$command), ".pl", sep = "")
        }
        return(mySoftwareCall)
    }
    if (!win()) {
        return(Sys.which(mySoftware$command))
    }
} 
