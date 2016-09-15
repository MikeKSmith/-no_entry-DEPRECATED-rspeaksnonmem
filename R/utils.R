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
    quotedStrings <- sapply(x, is.character)
    x1 <- x
    x1[quotedStrings] <- shQuote(x1[quotedStrings])
    x2 <- paste(names(x1), x1, sep = "=")
    x3 <- gsub("=TRUE", "", x2)
    x4 <- ifelse(length(grep("=FALSE", x3)) > 0, paste("no-", gsub("=FALSE", "", 
        x3), sep = ""), x3)
    x5 <- paste("-", x3, sep = "")
    paste0(x5, collapse = " ")
}

## -- from metrumrg runCommand.R --------------------------------------------- set
## up the call
execute <- function(command, intern = FALSE, minimized = FALSE, invisible = TRUE) {
    args <- list(command = command, intern = intern)
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
