#' Retrieves Data, Parameters, Task Properties from a NONMEM control file.
#'
#' @param RNMImportObject RNMImport object.
#' @param what 'Data', 'Parameters', 'Model', 'TaskProperties', 'All'.
#' @return Object from the NONMEM control stream corresponding to .
#' @examples
#' 
#' getNMObjects(foo, what='Data')
#' getNMObjects(foo, what='All')
getNMObjects <- function(RNMImportObject, 
                         what = c("Data", "Parameters", "TaskProperties", "All")) {
    ## TO BE WRITTEN
}
