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

validate_PsN_options <- function(command = NULL, psnOpts = NULL) {
  baseCommand <- ifelse(is.null(command), 
                        defineExecutable(tool = tool), 
                        defineExecutable(command = command))
  
  command <- paste(baseCommand, "-h ")
  psnArgs <- system(command, intern = TRUE)
  psnArgs <- stringr::str_trim(psnArgs)
  
  firstArg <- grep("^\\[ -h", psnArgs) + 1
  lastArg <- grep("^Options enclosed", psnArgs) - 2
  
  psnArgs <- psnArgs[firstArg:lastArg]
  optionalArgs <- grep("^\\[", psnArgs)
  mandatoryArgs <- grep("^--", psnArgs)
  psnArgs <- gsub("\\[", "", psnArgs)
  psnArgs <- gsub("\\]", "", psnArgs)
  psnArgs <- gsub("--", "", psnArgs)
  psnArgs <- gsub(" ", "", psnArgs)
  
  ## Check types
  psnArgs <- gsub("\\'string\\'", "is.character", psnArgs)  ## character
  psnArgs <- gsub("\\'integer\\'", "is.wholenumber", psnArgs)  ## wholenumber / integer
  psnArgs <- gsub("\\'number\\'", "is.double", psnArgs)  ## double precision
  psnArgs <- gsub("!", "=is.logical", psnArgs)  ## logical / boolean
  argName <- gsub("=.*", "", psnArgs)
  argType <- gsub(".*=", "", psnArgs)
  names(argType) <- argName
  argType[argType == argName] <- ""
  
  ## Check psnOpts names
  validName <- names(psnOpts) %in% argName
  if (!all(validName)) 
    warning(paste(names(psnOpts[!validName]),
                  "is not a valid PsN argument"))
  
  psnOpts <- psnOpts[validName]
  
  quotedStrings <- sapply(psnOpts, is.character)
  psnOpts[quotedStrings] <- shQuote(psnOpts[quotedStrings])
  checkpsnOpts <- psnOpts[argType[names(psnOpts)] != ""]
  validCommand <- paste(argType[names(checkpsnOpts)], "(", checkpsnOpts, ")", 
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
  
  list_to_PsNArgs(psnOpts)
  
}