#' Performs VPC of a model using PsN VPC
#'
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param addargs List of additional PsN command line arguments (format: argumentName = value or argumentName=TRUE/FALSE )
#' @param working.dir Working directory containing control stream and where output files should be stored
#' @param cleanup Whether to clean up additional NONMEM files and folders following estimation. Defaults to TRUE.
#' @return NONMEM estimation output files
#' @examples
#' VPC_PsN(modelFile='warfarin_PK_CONC_MKS.ctl', working.dir='./data')
#' @export

VPC_PsN <- function(command = NULL, modelFile = NULL, nsamp = 100, 
                    seed = 123456, addargs = NULL, cleanup = T, working.dir = NULL, ...) {

  addargsText <- list_to_PsNArgs(addargs)
  
  working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)
  
  # addargs <- ifelse(addargs==NULL, "autobin=T",addargs)
  
  baseCommand <- ifelse( is.null( command ), 
                         defineExecutable( tool = "VPC" , ... ) , 
                         defineExecutable( command=command, ... ))
  
  command <- paste(baseCommand, " ", shQuote(modelFile), " --samples=", 
                   nsamp, " --seed=", seed, " ", 
                   " --directory=", shQuote(working.dir),
                   if(cleanup) " --clean=2"," ",
                   addargsText, 
                   sep = "")
  cat(paste(command, "\n"))
  execute(command)
} 
