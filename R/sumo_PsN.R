#' Use PsN to summarise NONMEM output
#'
#' @param listFile NONMEM output file name
#' @param addargs Additional PsN command line arguments (text string)
#' @param working.dir Working directory containing control stream and where output files should be stored
#' @return PsN sumo output
#' @examples
#' execute_PsN(modelFile='warfarin_PK_CONC_MKS.ctl', working.dir='./data')
#'
sumo_PsN <- function(command = NULL, listFile = NULL,  addargs = NULL, 
                     working.dir = NULL, ...) {
  
  working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)
  
  baseCommand <- ifelse( is.null( command ), 
                         defineExecutable( tool = "sumo" , ... ) , 
                         defineExecutable( command=command, ... ))
  command <- paste(baseCommand, " ", shQuote(listFile), 
                   " --directory=", shQuote(working.dir),
                   " ", addargs, sep="")
  cat(paste(command, "\n"))
  execute(command = command)
} 
