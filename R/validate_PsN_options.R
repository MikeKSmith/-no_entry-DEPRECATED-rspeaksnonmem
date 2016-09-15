#' Checks valid options for PsN commands
#'
#' @param command PsN command.
#' @param psnOpts List of additional PsN command line options 
#' (format: optionName = value or optiontName=TRUE/FALSE )
#' @return character string of valid PsN arguments
#' @examples
#' validate_PsN_options(tool='execute', psnOpts = list(picky=TRUE, retries=3, 
#' tweak_inits=TRUE))
#' @export

validate_PsN_options <- function(command = NULL, psnOpts = NULL, 
                                 perl_options_command = "psn_options") {

  baseCommand <- ifelse(is.null(command), 
                        defineExecutable(tool = tool), 
                        defineExecutable(command = command))

  psnOptionCommand <- defineExecutable(command = "psn_options*")
  psnCommon <- system( paste(psnOptionCommand, "-h"), intern = TRUE)
  psnCommon <- parse_PsN_options(psnCommon)
  
  command <- paste(baseCommand, "-h ")
  psnOptions <- system(command, intern = TRUE)
  psnOptions <- parse_PsN_options(psnOptions)
  psnOptions$optional <- psnOptions$optional + length(psnCommon$optName)
  psnOptions$mandatory <- psnOptions$mandatory + length(psnCommon$optName)
  
  psnOptions <- mapply(c, psnCommon, psnOptions)
  
  ## Check psnOpts names
  validName <- names(psnOpts) %in% psnOptions$optName
  if (!all(validName)) 
    warning(paste(names(psnOpts[!validName]),
                  "is not a valid PsN argument"))
  
  psnOpts <- psnOpts[validName]
  
  quotedStrings <- sapply(psnOpts, is.character)
  psnOpts[quotedStrings] <- shQuote(psnOpts[quotedStrings])
  checkpsnOpts <- psnOpts[psnOptions$optType[names(psnOpts)] != ""]
  validCommand <- paste(psnOptions$optType[names(checkpsnOpts)], 
                        "(", checkpsnOpts, ")", 
                        sep = "")
  validArg <- sapply(validCommand, function(x) {
    eval(parse(text = x))
  })
  if (!all(validArg)) 
    warning(paste(checkpsnOpts[!validArg], 
                  "is not a valid value for the option", 
                  names(checkpsnOpts[!validArg])))
  
  checked <- checkpsnOpts[validArg]
  psnOpts <- psnOpts[!(names(psnOpts) %in% names(checkpsnOpts[!validArg]))]
  
  if ( !(psnOptions$optName[psnOptions$mandatory] %in% names(psnOpts) ) )
    warning(paste("Mandatory option",psnOptions$optName[psnOptions$mandatory],
                  "is not present in the provided option list"))
  
  list_to_PsNArgs(psnOpts)
  
}