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

  require(dplyr)
  
  if (is.null(command)) stop("Expecting a valid PsN command")
  
  psnOptionCommand <- defineExecutable(command = "psn_options*")
  psnCommon <- system( paste(psnOptionCommand, "-h"), intern = TRUE)
  psnCommon <- parse_PsN_options(psnCommon)
  
  command <- paste(command, "-h ")
  psnOptions <- system(command, intern = TRUE)
  psnOptions <- parse_PsN_options(psnOptions)

  psnOptions <- dplyr::union(psnOptions, psnCommon)
  
  ## Check psnOpts names
  matchedNames <- sapply(names(psnOpts),function(x)pmatch(x,psnOptions$name))
  validName <- !is.na(matchedNames)
  if (!all(validName)) 
    warning(paste(names(psnOpts[!validName]),
                  "is not a valid PsN argument",collapse = "\n"))
  if (!any(validName)) return("")

  psnOpts <- psnOpts[validName]
  names(psnOpts) <- psnOptions$name[matchedNames[validName]]
  
  quotedStrings <- sapply(psnOpts, is.character)
  psnOpts[quotedStrings] <- shQuote(psnOpts[quotedStrings])
  psnOptType <- psnOptions$type[matchedNames[validName]]
  checkOptType <- psnOptType
  checkOptType[psnOptType==""] <- "is.logical"
  checkArg <- paste(checkOptType, "(",psnOpts,")",sep="")
  validArg <- sapply(checkArg, function(x) {
    eval(parse(text = x))
  })
  if (!all(validArg)) 
    warning(paste(psnOpts[!validArg], 
                  "is not a valid value for the option", 
                  names(psnOpts[!validArg])))
  
  checked <- psnOpts[validArg]

  if ( any(psnOptions$mandatory) )
    if (!(psnOptions$name[psnOptions$mandatory] %in% names(psnOpts) ) )
    stop(paste("Mandatory option",psnOptions$name[psnOptions$mandatory],
                  "is not present in the provided option list"))

  if (length(checked)>0) {
  optList <- list(name = names(checked),
                  value = as.character(checked), 
                  type = psnOptType[validArg])
  
  list_to_PsNArgs(optList)
  } else return("")
}
