#' Use PsN to summarise NONMEM output
#'
#' @param listFile NONMEM output file name
#' @param addargs List of additional PsN command line arguments (format: argumentName = value or argumentName=TRUE/FALSE )
#' @param working.dir Working directory containing control stream and where output files should be stored
#' @return PsN sumo output
#' @examples
#' execute_PsN(modelFile='warfarin_PK_CONC_MKS.ctl', working.dir='./data')
#' @export

sumo_PsN <- function(command = NULL, listFile = NULL,  addargs = NULL, 
                     working.dir = NULL, ...) {

  addargsText <- ifelse(!is.null(addargs),list_to_PsNArgs(addargs), "")
  
  working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)
  
  baseCommand <- ifelse( is.null( command ), 
                         defineExecutable( tool = "sumo" , ... ) , 
                         defineExecutable( command=command, ... ))
  command <- paste(baseCommand, " ", shQuote(listFile), 
                   " --directory=", shQuote(working.dir),
                   " ", addargsText,
                   sep="")
  cat(paste(command, "\n"))
  execute(command = command)
} 
