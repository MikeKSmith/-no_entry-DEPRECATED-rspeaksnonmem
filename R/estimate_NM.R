#' Estimates parameters using NONMEM
#'
#' @param command Explicit command for running NONMEM as would be run at the
#'  shell/DOS prompt.
#' @param tool Tool name e.g. "nmfe". To be used in conjunction with 
#' \code{"installPath"} and \code{"version"}.
#' @param installPath Installation path for NONMEM / PsN. e.g. "c:/nm72"
#' @param version Version of NONMEM as a character string. e.g. "7.2"
#' @param modelFile NONMEM control stream file name (without extension)
#' @param lstFile NONMEM output file name. e.g. Run1
#' @param lstFileExtension NONMEM output file extension to be inferred from 
#' control stream name if an lstFile isn't explicitly given. Defaults to '.lst'.
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @param cleanup Whether to clean up additional NONMEM files and folders 
#' following estimation. Defaults to TRUE.
#' @return NONMEM estimation output files
#' @examples
#' estimate.NM(command = "c:/nm72/run/nmfe72.bat",
#' modelFile='Theophylline.mod', lstFile="Theophylline.lst",
#' working.dir='./data')

estimate_NM <- function(command = NULL, 
                        tool = NULL,
                        installPath = NULL,
                        version = NULL,
                        modelFile = NULL, 
                        lstFile = NULL, 
                        lstFileExtension = "lst", 
                        clean = 1) {
    
    baseCommand <- ifelse(is.null(command), 
                          defineExecutable(tool = tool,
                                           installPath = installPath,
                                           version = version), 
                          defineExecutable(command = command))
    lstFile <- ifelse(is.null(lstFile), 
                      paste(tools::file_path_sans_ext(modelFile), 
                            sub("\\.", "", lstFileExtension), sep = "."), 
                      lstFile)
    
    command <- paste(baseCommand, shQuote(modelFile), shQuote(lstFile))
    
    cat(paste(command, "\n"))
    execute(command)
    
    if (clean > 0) cleanup()
}
