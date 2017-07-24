#' utility functions used within the package
#'
#'
win <- function() {
    .Platform$OS.type == "windows"
}

# A version of Sys.which that might work better in Windows.  From
# http://stackoverflow.com/questions/34030087/how-to-find-correct-executable-with-sys-which-on-windows
Sys.which2 <- function(cmd) {
    stopifnot(length(cmd) == 1)
    if (.Platform$OS.type == "windows") {
        suppressWarnings({
            pathname <- shell(sprintf("where %s 2> NUL", cmd), intern = TRUE)[1]
        })
        if (!is.na(pathname))
            return(setNames(pathname, cmd))
    }
    Sys.which(cmd)
}

## Convert key-value pairs to PsN argument list
list_to_PsNArgs <- function(x) {
    x1 <- paste(x$name, x$value, sep = "=")
    x2 <- gsub("=TRUE", "", x1)
    x3 <- gsub("=FALSE","", x2)
    x3[x$type == "is.logical" & x$value == "FALSE"] <-
      paste("no-", x$name[x$type == "is.logical" & x$value == FALSE], sep = "")
    x4 <- x3[!(x$type == "" & x$value == FALSE)]
    x5 <- paste("-",x4,sep="")
    paste0(x5, collapse = " ")
}

## Parse PsN command options list
parse_PsN_options <- function(x){
  psnArgs <- stringr::str_trim(x)

  firstArg <- grep("^\\[ -h", psnArgs) + 1
  lastArg <- grep("^Options enclosed", psnArgs) - 2

  psnArgs <- psnArgs[firstArg:lastArg]
  optional <- regexpr ("^\\[", psnArgs)>0
  mandatory <- regexpr("^--", psnArgs)>0
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

  knownTypes <- c("is.character","is.wholenumber","is.double","is.logical")
  isKnown <- stringr::word(psnArgs,2,sep="=") %in% knownTypes
  argType[!isKnown] <- "!is.null"  ## If complex type then ensure non-missing.
  ## Need to refactor this though to check PsN Options where >1 type is possible.

  names(argType) <- argName
  argType[argType == argName] <- ""

  psnArgs <- data.frame(name = argName,
                  type = argType,
                  optional = optional,
                  mandatory = mandatory,
                  stringsAsFactors=F)
  return(psnArgs)
}

## -- from metrumrg runCommand.R
execute <- function(command,minimized = FALSE, invisible = TRUE) {
    args <- list(command = command)
    if (win())
        args <- c(args, list(minimized = minimized, invisible = invisible))
    # cat(command, file=file.path(rdir,glue(run,'.cat')),sep='\n',append=TRUE)
    result <- tryCatch(do.call(system, args), error = function(e) warning(e$message,
        call. = FALSE, immediate. = TRUE))
    # if (is.integer(result)) result <- paste('Run',run,'has exit code',result)
    # cat(result,file=file.path(rdir,glue(run,'.cat')),sep='\n',append=TRUE)
    return(result)  #visible
}

#' ---cleanup: function to remove NONMEM cruft.
#' Based on Andy Hooker's cleanup.R function https://github.com/andrewhooker/MBAOD/blob/master/R/cleanup.R
cleanup <- function(working.dir = NULL, pattern = NULL, remove.folders = F, ...) {

    working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)

    print("- Cleaning up..")

    # remove old files before new run
    files <- c("xml", "_L", "_R", "INTER", "LINK", "nul", "nmprd4p.mod", "FCON",
        ".exe", "FDATA", "FMSG", "fort.6", "FREPORT", "FSIZES", "FSTREAM", "FSUBS",
        "fsubs.f90", "fsubs.o", "FSUBS.MU.F90", "GFCOMPILE.BAT", "linkc", "nmfe72",
        "set", "newline", "gfortran", "prsizes", "trash", "compile", "matlab", "garbage.out")
    if (length(pattern) > 0)
        files <- c(files, pattern)

    foo <- sapply(files, function(x) unlink(file.path(working.dir, dir(pattern = x))))
    unlink(file.path(working.dir, "temp_dir"), recursive = T)

    # remove PsN folders
    if (remove.folders) {
        all <- list.files(all.files = F, full.names = T)
        alldirs <- all[file.info(all)$isdir]
        matchdirs <- alldirs
        if (length(pattern) > 0)
            matchdirs <- alldirs[grep(pattern, alldirs)]
        unlink(matchdirs, recursive = T, force = T)
    }
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) <
    tol

check.executable <- function(x){
  tools::file_ext(x) %in% c("bat","exe","pl")
}