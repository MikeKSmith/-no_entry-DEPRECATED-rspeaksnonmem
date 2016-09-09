#' Write out a parsed Model as a NONMEM control stream
#'
#' @param templateModel
#' @param parsedControl
#' @param modelFile
#' @param modelExtension
#' @param modelBlockNames
#' @return NONMEM estimation output files
#' @note RNMImport does not currently parse PRIOR blocks. So we treat these as part of the modelBlockNames
#' and return the contents of this block unchanged.
#' @examples
#' execute_PsN(modelFile='warfarin_PK_CONC_MKS', modelExtension='.ctl', working.dir='./data')
#' @export

writeNMControlStream <- function(templateModel, parsedControl, outputFile, outFileExtension = ".mod", 
                                 modelBlockNames = c("PK", "PRE", "SUB", "MOD", "DES", "ERR","PRI")) {
  
  #########################################################
  ###
  ###   Identify position of model elements in template
  ###   compared to RNMImport sequence
  ###
  #########################################################
  control <- templateModel
  
  ### Where do the various block statements occur?
  blockpos <- grep("^ *[$]", control)
  blocks <- control[blockpos]
  
  ## Drop commented out lines blocks<-blocks[-grep('[;]',blocks)] Get first 'word' to
  ## determine order
  blocks1 <- sub(" +.*", "", blocks)
  blocks2 <- sub("$", "", blocks1, fixed = T)
  orig1 <- data.frame(block = blocks2, line = blockpos, stringsAsFactors = F)
  orig2 <- orig1[!duplicated(orig1$block), ]
  
  blocks3 <- substr(orig2$block, 1, 3)
  orig.pos <- c(1:length(blocks3))
  orig <- data.frame(block.id = blocks3, orig.pos = orig.pos, orig.block = orig2$block, line = orig2$line, 
                     stringsAsFactors = F)
  
  ### Get list of objects from the parsed Control file
  control2 <- parsedControl
  control2Blocks <- substr( casefold( names( control2 ), upper = T ), 1, 3 )
  RNMI.pos <- c(1:length(control2Blocks))
  RNMI <- data.frame(block.id = control2Blocks, RNMI.pos = RNMI.pos, RNMI.block = names(parsedControl), 
                     stringsAsFactors = F)
  
  ## Match blocks in control file to items in the parsed list
  ctrlmerged <- merge(orig, RNMI, by = "block.id", all = T)
  ctrlmerged <- ctrlmerged[order(ctrlmerged$orig.pos), ]
  ctrlmerged$orig.block[ is.na( ctrlmerged$orig.block ) ] <- casefold(ctrlmerged$RNMI.block[ is.na( ctrlmerged$orig.block ) ], upper = T)
  
  ## Leave out model related blocks from parsedcontrol Will pick these up directly from Raw
  ## file.  This means that we do not expect user to update the model!
  otherBlocks <- ctrlmerged[!(ctrlmerged$block.id %in% modelBlockNames), ]
  control2 <- control2[otherBlocks$RNMI.block]
  
  ## If blocks appear in the original, but not RNMImport parsed version then create RNMImport
  ## blocks.  e.g. $DES
  
  modelBlockCode <- list(NULL)
  modelBlocks <- ctrlmerged[ctrlmerged$block.id %in% modelBlockNames, ]
  for (i in 1:nrow(modelBlocks)) {
    nextBlock <- ctrlmerged[modelBlocks$orig.pos[i] + 1, ]
    modelStart <- modelBlocks$line[i]
    modelEnd <- nextBlock$line - 1
    
    codeLines <- control[modelStart:modelEnd]
    codeLines <- paste(codeLines, "\n")
    ## Last line doesn't need the line break
    codeLines[length(codeLines)] <- sub("\\n","",codeLines[length(codeLines)])
    modelBlockCode[[i]] <- codeLines
    names(modelBlockCode)[[i]] <- modelBlocks$orig.block[i]
  }
  
  addBlocks <- list(NULL)
  missBlocks <- ctrlmerged[is.na(ctrlmerged$RNMI.pos) & !(ctrlmerged$block.id %in% modelBlockNames),]
  
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
  
  #########################################################
  ###
  ###   THETA
  ###
  #########################################################
  
  ### Change $THETA -Inf and Inf values to missing Change $THETA values = 0 to '0 FIX'
  Theta <- formatC(as.matrix(control2$Theta[,c("Lower","Est","Upper")]))
  Theta <- apply(Theta, 2, function(x) sub("^ *Inf", NA, x))
  Theta <- apply(Theta, 2, function(x) sub("^ *-Inf", NA, x))
  
  ## If initial value is zero then it must be fixed. NONMEM initial values cannot be zero.
  # control2$Theta[Theta[, 2] == 0, "FIX"] <- TRUE
  
  ## Prepare for printing out
  ## Combine $THETA bounds into usual NONMEM format e.g. (0, 0.5, ) OR 0.5 OR (,0.5,1000)
  Theta.txt <- NULL
  for (i in 1 : nrow(Theta) ){
    ## Handle FIXED Thetas
    if (control2$Theta[i,"FIX"]){
      Theta.txt[i] <- paste(Theta[i,2],"FIX")
    } else {
      ## Handle THETAs where only a single value is given for inits
      if ( is.na( Theta[i,1] ) & is.na( Theta[i,3] ) ){
        Theta.txt[i] <- Theta[i, 2]   
      } else {
        ## Else create lower and/or upper bounded THETAs
        Theta.txt[i] <- paste("(", paste(Theta[i,], collapse = ","), ")")
        Theta.txt[i] <- gsub("NA", "", Theta.txt[i])
      }
    }
  }
  
  Theta.txt <- paste(Theta.txt, ";", control2$Theta[,"comments"])
  thetaBlockName <- paste("$",ctrlmerged$orig.block[ctrlmerged$RNMI.block=="Theta"],sep="")
  control2$Theta <- paste(paste(thetaBlockName,"\n",sep=""),paste(Theta.txt, collapse="\n"))
  
  #########################################################
  ###
  ###   OMEGA
  ###
  #########################################################
  
  Omega.txt <- NULL
  if(!is.null(control2$Omega)){
    control2$Omega$initialMatrix <- NULL
    for (i in 1:length( control2$Omega ) ){
      x <- control2$Omega[[i]]
      if ( !is.null( x$block ) ){
        ## Print BLOCK(n)
        ## If SAME then don't print values just text
        x$block <- paste( paste("$OMEGA BLOCK(", x$block, ")", sep=""),if( x$SAME) "SAME","\n")
        if (!x$SAME){
          x$values[upper.tri(x$values)] <- ""
          x$values <- as.data.frame(x$values)
          x$values <- ifelse(!x$SAME,paste( apply( x$values, 1, paste, collapse=" "), collapse = "\n" ), NULL)
        }
        if (x$SAME) { x$values <- ""}
        ## If FIX then add this
        x$FIX <- ifelse( x$FIX , "FIX \n" , "\n")
        Omega.txt[[i]] <- paste0(list(x$block, x$values, x$FIX),collapse="")
      } else { 
        y <- data.frame(x)
        y$FIX <- ifelse(x$FIX, "FIX", "")
        y$comments <- ifelse( !is.na(x$comments), paste(";",x$comments), "" )
        out <- apply(y, 1, paste, collapse=" ")
        omegaBlockName <-  paste("$",ctrlmerged$orig.block[ctrlmerged$RNMI.block=="Omega"],sep="")
        Omega.txt[[i]] <- paste(paste(omegaBlockName,"\n",sep=""),paste(out,collapse=" \n"))
      }
    }
    Omega.txt <- paste( Omega.txt, collapse = "" )
  }
  ## Overwrite control2$Omega with Omega above.
  control2$Omega <- Omega.txt
  
  #########################################################
  ###
  ###   SIGMA
  ###
  #########################################################
  
  Sigma.txt <- NULL
  if(!is.null(control2$Sigma)){
    control2$Sigma$initialMatrix <- NULL
    for (i in 1:length( control2$Sigma ) ){
      x <- control2$Sigma[[i]]
      if ( !is.null( x$block ) ){
        ## Print BLOCK(n)
        ## If SAME then don't print values just text
        x$block <- paste( paste("$SIGMA BLOCK(", x$block, ")", sep=""),if( x$SAME) "SAME","\n")
        if (!x$SAME){
          x$values[upper.tri(x$values)] <- ""
          x$values <- as.data.frame(x$values)
          x$values <- ifelse(!x$SAME,paste( apply( x$values, 1, paste, collapse=" "), collapse = "\n" ), NULL)
        }
        if (x$SAME) { x$values <- ""}
        ## If FIX then add this
        x$FIX <- ifelse( x$FIX , "FIX \n" , "\n")
        Sigma.txt[[i]] <- paste0(list(x$block, x$values, x$fixed),collapse="")
      } else { 
        y <- data.frame(x)
        y$FIX <- ifelse(x$FIX, "FIX", "")
        y$comments <- ifelse( !is.na(y$comments), paste(";",y$comments), "")
        out <- apply(y, 1, paste, collapse=" ")
        sigmaBlockName <-  paste("$",ctrlmerged$orig.block[ctrlmerged$RNMI.block=="Sigma"],sep="")
        Sigma.txt[[i]] <- paste(paste(sigmaBlockName,"\n",sep=""), paste(out,collapse=" \n"))
      }
    }
    Sigma.txt <- paste( Sigma.txt, collapse = "" )
  }
  control2$Sigma <- Sigma.txt
  
  #########################################################
  ###
  ###   PREPARE FOR WRITING OUT
  ###
  #########################################################
  
  ## $INPUT records - Paste together the variables names and labels e.g. SID=ID TIME=TIME
  ## AMT=AMT BWT=DROP MDV=MDV DV=DV More detail than necessary / usual, but consistent with
  ## RNMImport object
  
  #### If the two are equal then write only one
  
  Input <- control2$Input[, "nmName"]
  diffInput <- control2$Input[, "nmName"] != control2$Input[, "Label"]
  if (any(diffInput)) {
    Input[diffInput] <- paste(control2$Input[diffInput, "nmName"], control2$Input[diffInput, 
                                                                                  "Label"], sep = "=")
  }
  
  control2$Input <- paste( paste("$",ctrlmerged$orig.block[ctrlmerged$RNMI.block=="Input"],sep="")," ",
                           paste(Input, collapse=" ")
  )
  
  ## $DATA records - Paste together commands and attributes e.g. THEO.DAT IGNORE=# etc.
  
  Data <- paste("'", control2$Data[1], "'", sep = "")
  
  if (control2$Data[2] != "NONE") {
    colnames(control2$Data)[2] <- "IGNORE"
    ignoreAccept <- paste(colnames(control2$Data), control2$Data, sep = "=")[c(2, 3)]
    ignoreAccept <- ignoreAccept[grep(".", control2$Data[c(2, 3)])]  ## Non-missing
    if (length( grep(";", ignoreAccept) ) > 0) {
      ignoreAccept <- unlist(strsplit(ignoreAccept, ";"))
      ignoreAccept2 <- sapply(ignoreAccept[-1], function(x) paste("\n IGNORE=", x, sep = ""))
      ignoreAccept <- c(ignoreAccept[1], ignoreAccept2)
    }
    ### Change $DATA REWIND statement to NOREWIND rather than REWIND=FALSE
    control2$Data[4] <- ifelse(control2$Data[4] == "FALSE", "NOREWIND", "")
    
    Data <- c(control2$Data[1], ignoreAccept)
  }
  control2$Data <- paste( paste("$",ctrlmerged$orig.block[ctrlmerged$RNMI.block=="Data"],sep="")," ",
                          paste(Data, collapse=" ")
  )
  
  ## Omit Data file commands that have no attributes
  
  ## Check for existence of $Tables in original code
  if (length(control2$Tables)) {
    ## Collect $TABLE variable strings, delete comma separator, append ONEHEADER NOPRINT
    ## statements
    tableBlockName <- paste("$",ctrlmerged$orig.block[ctrlmerged$RNMI.block=="Tables"],sep="") 
    Tables <- apply(control2$Table, 1, function(x) {
      paste( tableBlockName, " ",
             gsub(",", "", x[2]), " ONEHEADER NOPRINT FILE=", x[1], sep = "")
    })
    Tables <- gsub("ETA\\.", "ETA\\(", Tables, perl = T)
    Tables <- gsub("\\.", "\\)", Tables, perl = T)
  }
  control2$Tables <- paste(Tables, collapse="\n")
  
  ## Ensure that multiple $EST case has $EST for each line 
  ## First $Table statement doesn't need '$Table' since it comes from ctrlmerged if present
  estBlockName <- paste("$",ctrlmerged$orig.block[ctrlmerged$RNMI.block=="Estimates"],sep="")
  control2$Estimates <- paste(estBlockName, paste(control2$Estimates, collapse=paste("\n",estBlockName,sep="")))
  
  control2$Problem <- paste(paste("$",ctrlmerged$orig.block[ctrlmerged$RNMI.block=="Problem"],sep=""), control2$Problem)
  
  control2$Cov <- ifelse(grep("COV",ctrlmerged$orig.block)>0, "$COV\n",NULL)
  
  control3 <- list(NULL)
  for (i in 1:nrow(ctrlmerged)) {
    if (ctrlmerged$block.id[i] %in% otherBlocks$block.id) 
      control3[[i]] <- control2[[ctrlmerged$RNMI.block[i]]]
    if (ctrlmerged$block.id[i] %in% modelBlockNames) 
      control3[[i]] <- paste(modelBlockCode[[ctrlmerged$orig.block[i]]],collapse="")
    names(control3)[[i]] <- ctrlmerged$orig.block[i]
  }
  
  ##################################### Writing out the control statements
  
  ### PROBABLY NEEDS BETTER HANDLING OF ORDER OF BLOCKS IN THE NONMEM CODE USE RULES FROM
  ### NONMEM HELP GUIDES?  FOR NOW BASED ON ORDER IN ORIGINAL NM CODE IF ITEMS ADDED THROUGH
  ### updateMOG(...) THEN ADD THESE AT THE END?  USUALLY TABLE ITEMS
  
  model <- is.element(ctrlmerged$block.id, modelBlockNames)
  
  fileName <- ifelse(tools::file_ext(outputFile)=="", paste(outputFile, sub(".","",outFileExtension), sep = "."), outputFile)
  sink(file = fileName)
  for (i in 1:nrow(ctrlmerged)) {
    cat(paste("\n",control3[[i]]))
  }
  sink()
} 
