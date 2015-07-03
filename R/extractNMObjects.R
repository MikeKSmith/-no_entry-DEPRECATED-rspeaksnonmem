## ---getNMBlocks: identifies the various blocks of a NONMEM control stream
getNMBlocks <- function(RNMImportObject) {
    Raw <- RNMImportObject[[1]]
    blocks <- grep("^\\$", Raw)
    nextBlock <- c(blocks[-1], length(Raw))
    ## Drop commented out lines blocks<-blocks[-grep('[;]',blocks)] Get first 'word' to determine order
    blocks2 <- sub(" +.*", "", Raw[blocks])
    blocks3 <- sub("$", "", blocks2, fixed = T)
    data.frame(Blocks = blocks2, Search = blocks3, firstRow = blocks, nextBlockRow = nextBlock)
}

## ---getNMDataObjects: Reads the $DATA and $INPUT records and parses them
getNMDataObjects <- function(RNMImportObject) {
    Raw <- RNMImportObject[[1]]
    Parsed <- RNMImportObject[[4]][[1]]
    
    blockInfo <- getNMBlocks(RNMImportObject)
    rows <- unlist(blockInfo[as.character(blockInfo$Search) == "DATA", c("firstRow", "nextBlockRow")])
    rawDataRows <- Raw[rows[1]:(rows[2] - 1)]
    
    rows <- unlist(blockInfo[as.character(blockInfo$Search) == "INPUT", c("firstRow", "nextBlockRow")])
    rawInputRows <- Raw[rows[1]:(rows[2] - 1)]
    
    RAW <- c(rawDataRows, rawInputRows)
    if (length(grep("^\\;", RAW)) > 0) {
        RAW <- RAW[-grep("^\\;", RAW)]
    }
    
    list(RAW = RAW, HEADER = Parsed$Input, FILE = Parsed$Data)
}

## ---getNMParameterObjects: Reads the $THETA, $OMEGA, $SIGMA records and parses them
getNMParameterObjects <- function(RNMImportObject) {
    Raw <- RNMImportObject[[1]]
    Parsed <- RNMImportObject[[4]][[1]]
    
    blockInfo <- getNMBlocks(RNMImportObject)
    rows <- unlist(blockInfo[as.character(blockInfo$Search) == "THETA", c("firstRow", "nextBlockRow")])
    rawThetaRows <- Raw[rows[1]:(rows[2] - 1)]
    
    rows <- unlist(blockInfo[as.character(blockInfo$Search) == "OMEGA", c("firstRow", "nextBlockRow")])
    rawOmegaRows <- Raw[rows[1]:(rows[2] - 1)]
    
    rows <- unlist(blockInfo[as.character(blockInfo$Search) == "SIGMA", c("firstRow", "nextBlockRow")])
    rawSigmaRows <- Raw[rows[1]:(rows[2] - 1)]
    
    RAW <- c(rawThetaRows, rawOmegaRows, rawSigmaRows)
    if (length(grep("^\\;", RAW)) > 0) {
        RAW <- RAW[-grep("^\\;", RAW)]
    }
    
    list(RAW = RAW, STRUCTURAL = Parsed$Theta, VARIABILITY = list(IIV = Parsed$Omega, RUV = Parsed$Sigma))
}

## ---getNMTaskProperties: Reads the $EST, $COV, $TAB records and parses them
getNMTaskPropertiesObjects <- function(RNMImportObject) {
    Raw <- RNMImportObject[[1]]
    Parsed <- RNMImportObject[[4]][[1]]
    
    blockInfo <- getNMBlocks(RNMImportObject)
    rows <- unlist(blockInfo[as.character(blockInfo$Search) == "EST", c("firstRow", "nextBlockRow")])
    rawEstRows <- Raw[rows[1]:(rows[2] - 1)]
    
    rows <- unlist(blockInfo[as.character(blockInfo$Search) == "COV", c("firstRow", "nextBlockRow")])
    rawCovRows <- Raw[rows[1]:(rows[2] - 1)]
    
    rows <- unlist(blockInfo[as.character(blockInfo$Search) == "TAB", c("firstRow", "nextBlockRow")])
    rawTableRows <- Raw[rows[1]:(rows[2] - 1)]
    
    RAW <- c(rawEstRows, rawCovRows)
    if (length(grep("^\\;", RAW)) > 0) {
        RAW <- RAW[-grep("^\\;", RAW)]
    }
    
    list(RAW = RAW, TARGET_CODE = list(Parsed$Estimates, Parsed$Cov, Parsed$Tables))
} 
