#' Checks valid arguments for PsN commands
#'
#' @param tool PsN command
#' @param addargs List of additional PsN command line arguments 
#' (format: argumentName = value or argumentName=TRUE/FALSE )
#' @return character string of valid PsN arguments
#' @examples
#' validate_PsN_args(tool='execute', addargs = list(picky=TRUE, retries=3, 
#' tweak_inits=TRUE))
#' @export

validate_PsN_args <- function(tool = NULL, command = NULL, addargs = NULL) {
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
    
    ## Check addargs names
    validName <- names(addargs) %in% argName
    if (!all(validName)) 
        warning(paste(names(addargs[!validName]),
                      "is not a valid PsN argument"))
    
    addargs <- addargs[validName]
    
    quotedStrings <- sapply(addargs, is.character)
    addargs[quotedStrings] <- shQuote(addargs[quotedStrings])
    checkAddargs <- addargs[argType[names(addargs)] != ""]
    validCommand <- paste(argType[names(checkAddargs)], "(", checkAddargs, ")", 
        sep = "")
    validArg <- sapply(validCommand, function(x) {
        eval(parse(text = x))
    })
    if (!all(validArg)) 
        warning(paste(checkAddargs[!validArg], 
                      "is not a valid value for the option", 
                      names(checkAddargs[!validArg])))
    
    checked <- checkAddargs[validArg]
    addargs <- addargs[!(names(addargs) %in% names(checkAddargs[!validArg]))]
    
    list_to_PsNArgs(addargs)
    
}