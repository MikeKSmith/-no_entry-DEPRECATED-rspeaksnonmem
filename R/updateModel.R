#' Change elements of the Model based on inputs
#'
#' @param parsedObject R object containing parsed NONMEM objects
#' @param problem $Problem (NONMEM $PROB) list within the R Model Object
#' @param theta $Theta list (NONEMM $THETA) within the R Model Object
#' @param omega $Omega list (NONMEM $OMEGA) within the R Model Object
#' @param sigma $Sigma list (NONMEM $SIGMA) within the R Model Object
#' @param task $Estimates list (NONMEM $EST, $SIM) within the R Model Object
#' @param data $Data list (NONMEM $DATA) within the R Model Object
#' @param dataNames $Input list (NONMEM $INPUT) within the R Model Object
#' @param tables $Tables list (NONMEM $TABLES) within the R Model Object
#' @param addPsNHeader Logical - whether to include PsN Header information. 
#' Default FALSE.
#' @return NONMEM estimation output files
#' @examples
#' @export

updateModel <- function(parsedObject, problem = parsedObject$Problem, 
                        theta = parsedObject$Theta, omega = parsedObject$Omega, 
                        sigma = parsedObject$Sigma, task = parsedObject$Estimates,
                        data = parsedObject$Data, dataNames = parsedObject$Input, 
                        tables = parsedObject$Tables, 
    addPsNHeader = F, runno = 0, ...) {
    newObject <- parsedObject
    newObject$Problem <- problem
    newObject$Theta <- theta
    newObject$Omega <- omega
    newObject$Sigma <- sigma
    newObject$Estimates <- task
    newObject$Data <- data
    newObject$Input <- dataNames
    newObject$Tables <- tables
    
    newObject$Tables$File <- gsub("[0:9]", runno, parsedObject$Tables$File)
    
    if (addPsNHeader) 
        newObject$Problem <- PsNHeader(parsedObject, ...)
    return(newObject)
}
