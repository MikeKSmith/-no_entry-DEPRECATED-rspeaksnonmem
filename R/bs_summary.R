#' Parses the PsN bootstrap results file (bootstrap_results.csv)
#'
#' @param fileName fileName for the bootstrap results file. Default 
#' 'bootstrap_results.csv'
#' @param bs.dir Directory containing control stream and where bootstrap 
#' results file (bootstrap_results.csv) is stored. Defaults to './bootstrap_dir1' 
#' as PsN default.
#' @return list containing parsed blocks from bootstrap_results.csv
#' @examples
#'
bs_summary <- function(fileName = "bootstrap_results.csv", bs.dir = "./bootstrap_dir1") {
    
    text <- readLines(file.path(bs.dir, fileName))
    
    diagnostics <- read.csv(fileName, skip = grep("^diagnostic", text), header = T, 
        nrows = 1)[-1]
    
    means <- read.csv(fileName, skip = grep("^means", text) + 1, header = F, nrows = 1)[-1]
    names(means) <- gsub("\\s", "", read.csv(fileName, skip = grep("^means", text), 
        nrows = 1, header = F, stringsAsFactors = F)[-1])
    
    bias <- read.csv(fileName, skip = grep("^bias", text) + 1, header = F, nrows = 1)[-1]
    names(bias) <- gsub("\\s", "", read.csv(fileName, skip = grep("^bias", text), 
        nrows = 1, header = F, stringsAsFactors = F)[-1])
    
    se_CI <- read.csv(fileName, skip = grep("^standard.error.confidence", text) + 
        1, nrows = 8, header = F, row.names = 1)
    names(se_CI) <- gsub("\\s", "", read.csv(fileName, skip = grep("^standard.error.confidence", 
        text), nrows = 1, header = F, stringsAsFactors = F)[-1])
    row.names(se_CI) <- gsub("\\s", "", row.names(se_CI))
    
    se <- read.csv(fileName, skip = grep("^standard.error", text) + 1, header = F, 
        nrows = 1)[-1]
    names(se) <- gsub("\\s", "", read.csv(fileName, skip = grep("^standard.error", 
        text), nrows = 1, header = F, stringsAsFactors = F)[-1])
    
    medians <- read.csv(fileName, skip = grep("^medians", text) + 1, header = F, 
        nrows = 1)[-1]
    names(medians) <- gsub("\\s", "", read.csv(fileName, skip = grep("^medians", 
        text), nrows = 1, header = F, stringsAsFactors = F)[-1])
    
    perc_CI <- read.csv(fileName, skip = grep("^percentile", text) + 1, nrows = 8, 
        header = F, row.names = 1)
    names(perc_CI) <- gsub("\\s", "", read.csv(fileName, skip = grep("^percentile", 
        text), nrows = 1, header = F, stringsAsFactors = F)[-1])
    row.names(perc_CI) <- gsub("\\s", "", row.names(perc_CI))
    
    out <- list(diagnostics = diagnostics, means = means, bias = bias, se_CI = se_CI, 
        se = se, medians = medians, perc_CI = perc_CI)
    out
}
