#' Checks valid options for PsN commands
#'
#' @param command PsN command.
#' @param tool PsN tool.
#' @param installPath Installation path for Perl / PsN
#' @param version Version of PsN
#' @param psnOpts List of additional PsN command line options 
#' (format: optionName = value or optiontName=TRUE/FALSE )
#' @details Either specify an explicit command to run at the system prompt 
#' or specify a combination of tool, installPath and version.
#' @return character string of valid PsN arguments
#' @examples
#' validate_PsN_options(command='execute', 
#' psnOpts = list(picky=TRUE, retries=3, 
#' tweak_inits=TRUE))
#' @export

validate_PsN_options <- function(command = NULL, 
                                 tool = NULL,
                                 installPath = NULL,
                                 version = NULL,
                                 psnOpts = NULL) {

  require(dplyr)
  require(stringr)
  
  if(is.null(tool) && is.null(command)) stop("One of tool or command must be specified")
  
  # If using tool then specify installPath and version
  if (is.null(command) && !is.null(tool)){
    if (is.null(installPath)) stop("If using tool, please specify an installPath")
    if (is.null(version)) stop("If using tool, please specify a version")      
  }
  
  psnOptionCommand <- ifelse( is.null(command), 
                              defineExecutable(tool = "psn_options", 
                                               version = version,
                                               installPath = installPath),
                              defineExecutable(command = file.path(
                                dirname(
                                  stringr::word(command,1)
                                  ),"psn_options")
                                )
                              )

  psnCommon <- system(command = paste(psnOptionCommand,"-h"), intern = T)
  psnCommon <- parse_PsN_options(psnCommon)
  
  command <- ifelse(is.null(command),
                    paste(
                      defineExecutable(tool = tool, 
                                       version = version,
                                       installPath = installPath),
                      "-h"
                    ),
                    paste(command, "-h ")
  )
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
