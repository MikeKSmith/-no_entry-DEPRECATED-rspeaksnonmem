#' Write out a parsed Model as a NONMEM control stream
#'
#' @param templateModel
#' @param parsedControl
#' @param modelFile
#' @param modelExtension
#' @param modelBlockNames
#' @return NONMEM estimation output files
#' @examples
#' execute.PsN(modelFile='warfarin_PK_CONC_MKS', modelExtension='.ctl', working.dir='./data')
#'### ----writeControlText: Write out a parsed Model as a NONMEM control stream

writeControlText <- function(templateModel, parsedControl, modelFile, modelExtension = ".mod", modelBlockNames = c("PK", "PRE", "SUB", "MOD", "DES", 
    "ERR")) {
    
    ### Get RAW NM control stream items
    control <- templateModel
    
    ### Where do the various block statements occur?
    blockpos <- grep("^ *[$]", control)
    blocks <- control[blockpos]
    
    ## Drop commented out lines blocks<-blocks[-grep('[;]',blocks)] Get first 'word' to determine order
    blocks1 <- sub(" +.*", "", blocks)
    blocks2 <- sub("$", "", blocks1, fixed = T)
    orig1 <- data.frame(block = blocks2, line = blockpos, stringsAsFactors = F)
    orig2 <- orig1[!duplicated(orig1$block), ]
    
    blocks3 <- substr(orig2$block, 1, 3)
    orig.pos <- c(1:length(blocks3))
    orig <- data.frame(block.id = blocks3, orig.pos = orig.pos, orig.block = orig2$block, line = orig2$line, stringsAsFactors = F)
    
    ### Get list of objects from the parsed Control file
    control2 <- parsedControl
    control2Blocks <- substr(casefold(names(control2), upper = T), 1, 3)
    RNMI.pos <- c(1:length(control2Blocks))
    RNMI <- data.frame(block.id = control2Blocks, RNMI.pos = RNMI.pos, RNMI.block = names(parsedControl), stringsAsFactors = F)
    
    ## Match blocks in control file to items in the parsed list
    ctrlmerged <- merge(orig, RNMI, by = "block.id", all = T)
    ctrlmerged <- ctrlmerged[order(ctrlmerged$orig.pos), ]
    ctrlmerged$orig.block[is.na(ctrlmerged$orig.block)] <- casefold(ctrlmerged$RNMI.block[is.na(ctrlmerged$orig.block)], upper = T)
    
    ## Leave out model related blocks from parsedcontrol Will pick these up directly from Raw file.  This means that we do not expect user to update the
    ## model!
    otherBlocks <- ctrlmerged[!(ctrlmerged$block.id %in% modelBlockNames), ]
    control2 <- control2[otherBlocks$RNMI.block]
    
    ## If blocks appear in the original, but not RNMImport parsed version then create RNMImport blocks.  e.g. $DES
    
    modelBlockCode <- list(NULL)
    modelBlocks <- ctrlmerged[ctrlmerged$block.id %in% modelBlockNames, ]
    for (i in 1:nrow(modelBlocks)) {
        nextBlock <- ctrlmerged[modelBlocks$orig.pos[i] + 1, ]
        modelStart <- modelBlocks$line[i]
        modelEnd <- nextBlock$line - 1
        
        codeLines <- control[modelStart:modelEnd]
        codeLines <- paste(codeLines, "\n")
        modelBlockCode[[i]] <- codeLines
        names(modelBlockCode)[[i]] <- modelBlocks$orig.block[i]
    }
    
    addBlocks <- list(NULL)
    missBlocks <- ctrlmerged[is.na(ctrlmerged$RNMI.pos) & !(ctrlmerged$block.id %in% modelBlockNames), ]
    if (nrow(missBlocks) > 0) {
        for (i in 1:nrow(missBlocks)) {
            nextBlock <- ctrlmerged[missBlocks$orig.pos[i] + 1, ]
            missStart <- missBlocks$line[i] + 1  ## NOTE! The +1 here might cause trouble!
            missEnd <- nextBlock$line - 1
            
            codeLines <- control[missStart:missEnd]
            addBlocks[[i]] <- codeLines
            names(addBlocks)[[i]] <- as.character(missBlocks$block.id[i])
            newRNMIpos <- max(ctrlmerged$RNMI.pos, na.rm = T) + i
            ctrlmerged[ctrlmerged$block.id == missBlocks[i, "block.id"], "RNMI.pos"] <- newRNMIpos
        }
    }
    
    ### Change $THETA -Inf and Inf values to missing Change $THETA values = 0 to '0 FIX'
    control2$Theta <- formatC(control2$Theta)
    control2$Theta <- apply(control2$Theta, 2, function(x) sub("^ *Inf", NA, x))
    control2$Theta <- apply(control2$Theta, 2, function(x) sub("^ *-Inf", NA, x))
    control2$Theta[control2$Theta[, 1] == control2$Theta[, 3], c(1, 3)] <- NA
    control2$Theta[control2$Theta[, 2] == 0, c(1, 3)] <- NA
    control2$Theta[control2$Theta[, 2] == 0, 2] <- "0 FIX"
    
    ### Change $OMEGA values = 0 to '0 FIX' THIS NEEDS WORK!!!  Turn Omega matrix into diagonal etc.  and handle block structures
    
    Omega <- NULL
    
    ## Are only diagonals filled??
    OmegaDiag <- sum(control2$Omega[lower.tri(control2$Omega, diag = F)]) == 0
    if (OmegaDiag) {
        Omega.blocksize <- NULL
        Omega <- diag(control2$Omega)
        Omega[Omega == 0] <- "0 FIX"
        Omega <- sapply(Omega, function(x) paste(x, "\n"))
    }
    if (!OmegaDiag) {
        ## Which Omegas are BLOCK
        Corr <- (apply(control2$Omega, 2, sum) - diag(control2$Omega)) != 0
        block <- paste("BLOCK(", sum(Corr), ")\n", sep = "")
        Omega1 <- control2$Omega[Corr, Corr]
        Omega.1 <- paste(Omega1[lower.tri(Omega1, diag = T)], "\n")
        Omega <- list(block, Omega.1)
        if (sum(Corr) != length(diag(control2$Omega))) {
            Omega.2 <- diag(control2$Omega[!Corr, !Corr])
            Omega.2[Omega.2 == 0] <- "0 FIX"
            Omega.2 <- sapply(Omega.2, function(x) paste(x, "\n"))
            Omega <- list(block, Omega.1, "\n$OMEGA\n", Omega.2)
        }
    }
    
    ## Overwrite control2$Omega with Omega above.
    control2$Omega <- Omega
    names(control2$Omega) <- NULL
    
    Sigma <- NULL
    
    ## Are only diagonals filled??
    sigmaVector <- is.vector(control2$Sigma)
    if (sigmaVector) {
        Sigma <- control2$Sigma
        Sigma[Sigma == 0] <- "0 FIX"
        Sigma <- sapply(Sigma, function(x) paste(x, "\n"))
    }
    if (!sigmaVector) {
        SigmaDiag <- sum(control2$Sigma[lower.tri(control2$Sigma, diag = F)]) == 0
        if (SigmaDiag) {
            Sigma.blocksize <- NULL
            Sigma <- diag(control2$Sigma)
            Sigma[Sigma == 0] <- "0 FIX"
            Sigma <- sapply(Sigma, function(x) paste(x, "\n"))
        }
        if (!SigmaDiag) {
            ## Which Sigmas are BLOCK
            Corr <- (apply(control2$Sigma, 2, sum) - diag(control2$Sigma)) != 0
            block <- paste("BLOCK(", sum(Corr), ")\n", sep = "")
            Sigma1 <- control2$Sigma[Corr, Corr]
            Sigma.1 <- paste(Sigma1[lower.tri(Sigma1, diag = T)], "\n")
            Sigma <- list(block, Sigma.1)
            if (sum(Corr) != length(diag(control2$Sigma))) {
                Sigma.2 <- diag(control2$Sigma[!Corr, !Corr])
                Sigma.2[Sigma.2 == 0] <- "0 FIX"
                Sigma.2 <- sapply(Sigma.2, function(x) paste(x, "\n"))
                Sigma <- list(block, Sigma.1, "\n$Sigma\n", Sigma.2)
            }
        }
    }
    
    control2$Sigma <- Sigma
    names(control2$Sigma) <- NULL
    
    #################################################################### PREPARE ITEMS IN CONTROL2 FOR WRITING OUT
    
    ## $INPUT records - Paste together the variables names and labels e.g. SID=ID TIME=TIME AMT=AMT BWT=DROP MDV=MDV DV=DV More detail than necessary /
    ## usual, but consistent with RNMImport object
    
    #### If the two are equal then write only one
    
    Input <- control2$Input[, "nmName"]
    diffInput <- control2$Input[, "nmName"] != control2$Input[, "Label"]
    if (any(diffInput)) {
        Input[diffInput] <- paste(control2$Input[diffInput, "nmName"], control2$Input[diffInput, "Label"], sep = "=")
    }
    
    control2$Input <- Input
    
    ## $DATA records - Paste together commands and attributes e.g. THEO.DAT IGNORE=# etc.
    
    Data <- paste("'", control2$Data[1], "'", sep = "")
    
    if (control2$Data[2] != "NONE") {
        colnames(control2$Data)[2] <- "IGNORE"
        ignoreAccept <- paste(colnames(control2$Data), control2$Data, sep = "=")[c(2, 3)]
        ignoreAccept <- ignoreAccept[grep(".", control2$Data[c(2, 3)])]  ## Non-missing
        if (grep(";", ignoreAccept) > 0) {
            ignoreAccept <- unlist(strsplit(ignoreAccept, ";"))
            ignoreAccept2 <- sapply(ignoreAccept[-1], function(x) paste("\n IGNORE=", x, sep = ""))
            ignoreAccept <- c(ignoreAccept[1], ignoreAccept2)
        }
        ### Change $DATA REWIND statement to NOREWIND rather than REWIND=FALSE
        control2$Data[4] <- ifelse(control2$Data[4] == "FALSE", "NOREWIND", "")
        
        Data <- c(control2$Data[1], ignoreAccept)
    }
    control2$Data <- Data
    
    ## Omit Data file commands that have no attributes
    
    ## Combine $THETA bounds into usual NONMEM format e.g. (0, 0.5, ) OR 0.5 OR (,0.5,1000)
    Theta <- paste("(", apply(control2$Theta, 1, function(x) {
        paste(x, collapse = ",")
    }), ")\n")
    Theta <- gsub("NA", "", Theta)
    Theta[is.na(control2$Theta[, 1]) & is.na(control2$Theta[, 3])] <- paste(control2$Theta[is.na(control2$Theta[, 1]) & is.na(control2$Theta[, 3]), 2], 
        "\n")
    
    control2$Theta <- Theta
    
    ## Prepare $OMEGA for printings
    
    control2$Omega <- print(unlist(control2$Omega, as.character))
    
    ## Check for existence of $Tables in original code
    if (length(control2$Tables)) {
        ## Collect $TABLE variable strings, delete comma separator, append ONEHEADER NOPRINT statements
        Tables <- apply(control2$Table, 1, function(x) {
            paste("$TABLE ", gsub(",", "", x[2]), " ONEHEADER NOPRINT FILE=", x[1], "\n", sep = "")
        })
        ## First $Table statement doesn't need '$Table' since it comes from ctrlmerged if present
        if (!is.na(ctrlmerged$orig.block[ctrlmerged$block.id == "TAB"])) 
            Tables[1] <- sub("^\\$TABLE", "", Tables[1], perl = T)
        Tables <- gsub("ETA\\.", "ETA\\(", Tables, perl = T)
        Tables <- gsub("\\.", "\\)", Tables, perl = T)
        control2$Tables <- Tables
    }
    
    control3 <- list(NULL)
    for (i in 1:nrow(ctrlmerged)) {
        if (ctrlmerged$block.id[i] %in% otherBlocks$block.id) 
            control3[[i]] <- control2[[ctrlmerged$RNMI.block[i]]]
        if (ctrlmerged$block.id[i] %in% modelBlockNames) 
            control3[[i]] <- modelBlockCode[[ctrlmerged$orig.block[i]]]
        names(control3)[[i]] <- ctrlmerged$orig.block[i]
    }
    
    
    ##################################### Writing out the control statements
    
    ### PROBABLY NEEDS BETTER HANDLING OF ORDER OF BLOCKS IN THE NONMEM CODE USE RULES FROM NONMEM HELP GUIDES?  FOR NOW BASED ON ORDER IN ORIGINAL NM CODE
    ### IF ITEMS ADDED THROUGH updateMOG(...) THEN ADD THESE AT THE END?  USUALLY TABLE ITEMS
    
    ## 'special' blocks need $ statement on one line and content below
    special <- is.element(ctrlmerged$block.id, c("PK", "PRED", "ERR", "THE", "OME", "SIG", "DES", "MOD"))
    model <- is.element(ctrlmerged$block.id, modelBlockNames)
    ctrlmerged$orig.block[special] <- paste(ctrlmerged$orig.block[special], "\n")
    ctrlmerged$orig.block[model] <- ""
    
    sink(file = paste(modelFile, modelExtension, sep = ""))
    for (i in 1:nrow(ctrlmerged)) {
        if (!ctrlmerged$block.id[i] %in% modelBlockNames) 
            cat(paste("$", ctrlmerged$orig.block[i], " ", sep = ""))
        cat(paste(cat(control3[[i]]), "\n"))
    }
    sink()
} 
