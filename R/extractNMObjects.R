## ---getNMBlocks: identifies the various blocks of a NONMEM control stream
getNMBlocks <- function(RNMImportObject) {
    Raw <- RNMImportObject$Raw
    blocks <- grep("\\$", Raw)
    nextBlock <- c(blocks[-1], length(Raw))
    ## Drop commented out lines blocks<-blocks[-grep('[;]',blocks)] Get first 'word'
    ## to determine order
    blocks2 <- sub(" +.*", "", Raw[blocks])
    blocks3 <- sub("$", "", blocks2, fixed = T)
    data.frame(Blocks = blocks2, Search = blocks3, firstRow = blocks, nextBlockRow = nextBlock)
}

## ---getNMDataObjects: Reads the $DATA and $INPUT records and parses them
getNMDataObjects <- function(RNMImportObject, problemNo = 1) {
    Raw <- RNMImportObject$Raw
    Parsed <- RNMImportObject$problemContents[[problemNo]]
    
    blockInfo <- getNMBlocks(RNMImportObject)
    ## What rows correspond to each parameter type
    rowsData <- data.frame(blockInfo[grep("DAT", as.character(blockInfo$Search)), 
        c("firstRow", "nextBlockRow")], row.names = NULL)
    rowsInput <- data.frame(blockInfo[grep("INP", as.character(blockInfo$Search)), 
        c("firstRow", "nextBlockRow")], row.names = NULL)
    
    ## Extract DATA rows
    rawDataRows <- NULL
    if (length(grep("DAT", as.character(blockInfo$Search))) > 0) {
        rawDataRows <- apply(rowsData, 1, function(x) Raw[x[1]:(x[2] - 1)])
    }
    
    ## Extract INPUT rows
    rawInputRows <- NULL
    if (length(grep("OME", as.character(blockInfo$Search))) > 0) {
        rawInputRows <- apply(rowsInput, 1, function(x) Raw[x[1]:(x[2] - 1)])
    }
    
    RAW <- unlist(c(rawInputRows, rawDataRows))
    if (length(grep("^\\;", RAW)) > 0) {
        RAW <- RAW[-grep("^\\;", RAW)]
    }
    
    list(RAW = RAW, INPUT = Parsed$Input, DATA = Parsed$Data)
}

## ---getNMParameterObjects: Reads the $THETA, $OMEGA, $SIGMA records and parses
## them
getNMParameterObjects <- function(RNMImportObject, problemNo = 1) {
    Raw <- RNMImportObject$Raw
    Parsed <- RNMImportObject$problemContents[[problemNo]]
    
    blockInfo <- getNMBlocks(RNMImportObject)
    
    ## What rows correspond to each parameter type
    rowsTheta <- data.frame(blockInfo[grep("THE", as.character(blockInfo$Search)), 
        c("firstRow", "nextBlockRow")], row.names = NULL)
    rowsOmega <- data.frame(blockInfo[grep("OME", as.character(blockInfo$Search)), 
        c("firstRow", "nextBlockRow")], row.names = NULL)
    rowsSigma <- data.frame(blockInfo[grep("SIG", as.character(blockInfo$Search)), 
        c("firstRow", "nextBlockRow")], row.names = NULL)
    
    ## Extract THETA rows (if there are any)
    rawThetaRows <- NULL
    if (length(grep("THE", as.character(blockInfo$Search))) > 0) {
        rawThetaRows <- apply(rowsTheta, 1, function(x) Raw[x[1]:(x[2] - 1)])
    }
    
    ## Extract OMEGA rows (if there are any)
    rawOmegaRows <- NULL
    if (length(grep("OME", as.character(blockInfo$Search))) > 0) {
        rawOmegaRows <- apply(rowsOmega, 1, function(x) Raw[x[1]:(x[2] - 1)])
    }
    
    ## Extract SIGMA rows (if there are any)
    rawSigmaRows <- NULL
    if (length(grep("SIG", as.character(blockInfo$Search))) > 0) {
        rawSigmaRows <- apply(rowsSigma, 1, function(x) Raw[x[1]:(x[2] - 1)])
    }
    
    RAW <- unlist(c(rawThetaRows, rawOmegaRows, rawSigmaRows))
    if (length(grep("^\\;", RAW)) > 0) {
        RAW <- RAW[-grep("^\\;", RAW)]
    }
    
    list(RAW = RAW, THETA = Parsed$Theta, OMEGA = Parsed$Omega, SIGMA = Parsed$Sigma)
}

## ---getNMTaskProperties: Reads the $EST, $COV, $TAB records and parses them
getNMTaskPropertiesObjects <- function(RNMImportObject, problemNo = 1) {
    Raw <- RNMImportObject$Raw
    Parsed <- RNMImportObject$problemContents[[problemNo]]
    
    blockInfo <- getNMBlocks(RNMImportObject)
    
    ## What rows correspond to each parameter type
    rowsEST <- data.frame(blockInfo[grep("EST", as.character(blockInfo$Search)), 
        c("firstRow", "nextBlockRow")], row.names = NULL)
    rowsTAB <- data.frame(blockInfo[grep("TAB", as.character(blockInfo$Search)), 
        c("firstRow", "nextBlockRow")], row.names = NULL)
    rowsCOV <- data.frame(blockInfo[grep("COV", as.character(blockInfo$Search)), 
        c("firstRow", "nextBlockRow")], row.names = NULL)
    
    ## Extract EST rows (if there are any)
    rawEstRows <- NULL
    if (length(grep("EST", as.character(blockInfo$Search))) > 0) {
        rawEstRows <- apply(rowsEST, 1, function(x) Raw[x[1]:if (x[2] > x[1]) 
            (x[2] - 1) else (x[2])])
    }
    
    ## Extract TAB rows (if there are any)
    rawTabRows <- NULL
    if (length(grep("TAB", as.character(blockInfo$Search))) > 0) {
        rawTabRows <- apply(rowsTAB, 1, function(x) Raw[x[1]:if (x[2] > x[1]) 
            (x[2] - 1) else (x[2])])
    }
    
    ## Extract COV rows (if there are any)
    rawCovRows <- NULL
    if (length(grep("COV", as.character(blockInfo$Search))) > 0) {
        rawCovRows <- apply(rowsCOV, 1, function(x) Raw[x[1]:if (x[2] > x[1]) 
            (x[2] - 1) else (x[2])])
    }
    
    RAW <- unlist(c(rawEstRows, rawTabRows, rawCovRows))
    
    if (length(grep("^\\;", RAW)) > 0) {
        RAW <- RAW[-grep("^\\;", RAW)]
    }
    
    list(RAW = RAW, ESTIMATES = Parsed$Estimates, COV = Parsed$Cov, TABLES = Parsed$Tables)
}
