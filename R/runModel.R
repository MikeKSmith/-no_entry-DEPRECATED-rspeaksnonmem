#' Runs a complete workflow for a given Model Object
#'
#' @param modelFile NONMEM control stream file name (without extension)
#' @param modelExtension NONMEM control stream file extension. Defaults to '.mod'
#' @param addargs Additional PsN command line arguments (text string)
#' @param working.dir Working directory containing control stream and where 
#' output files should be stored
#' @param cleanup Whether to clean up additional NONMEM files and folders 
#' following estimation. Defaults to TRUE.
#' @return NONMEM estimation output files
#' @examples
#' execute_PsN(modelFile='warfarin_PK_CONC_MKS', modelExtension='.ctl', 
#' working.dir='./data')
#'
#'
runModel <- function(MOGobj, cleanup = T, diagnostics = T, VPC = F, 
                     bootstrap = F, runno = NULL, outFormat = NULL, 
                     templateModel = NULL, nsamp = 100, working.dir = NULL) {
    
  working.dir <- ifelse(is.null(working.dir), getwd(), working.dir)
    
    #' Initialise output list
    output <- as.list(NULL)
    
    #' Find the run number from the Tables statements sdtab etc.
    if (!length(runno) > 0) 
        runno <- as.numeric(gsub("[a-z]", "", MOGobj$Table[1, "File"]))
    
    #' Create the run directory
    #' Copy the dataset into the run directory
    #' Change working directory to the run directory
    runpath <- file.path(getwd(), paste("Run", runno, sep = ""))
    dir.create(runpath)
    file.copy(MOGobj$Data[, "File"], file.path(runpath))
    
    #' Using 'Run<nn>>' convention for temporary Run files
    ctlFilename <- paste("Run", runno, sep = "")
    fileNameRoot <- gsub("\\..*", "", ctlFilename)
    
    #' Writing out the new control stream
    writeControlText(templateModel = templateModel, parsedControl = MOGobj, modelFile = file.path(getwd(), 
        ctlFilename))
    
    #' estimate the model using PsN
    #' -------------------------
    execute_PsN(modelFile = ctlFilename, 
                psnOpts = list(retries = 3, tweak_inits = TRUE))
    
    # #' Use RNMImport to read the output files
    outNM <- RNMImport::importNm(paste(ctlFilename, ".mod", sep = ""))
    output$RNMImport <- outNM
    #' Model diagnostics
    #' -------------------------
    #' Here Xpose reads the NONMEM output from the run and creates an Xpose database object.
    
    #' If using png or PDF output then set up the filenaming convention
    #' Will be Run<<nn>>GOF<<pageNN>>.png or .pdf
    if (length(outFormat) > 0) {
        if (outFormat == "png") 
            png(file = paste("Run", runno, "GOF", "%d.png", sep = ""))
        if (outFormat == "pdf") 
            pdf(file = paste("Run", runno, "GOF", "%d.pdf", sep = ""))
    }
    basicGOF_Xpose()
    
    #' VPC simulation-based diagnostics using PsN
    #' -------------------------
    #' `VPC_PsN` is a wrapper to PsN's VPC function and simply passes the appropriate argument through to PsN.
    #' Additional arguments for VPC using PsN can be passed as part of the `addargs` string.
    
    if (VPC) {
        VPC_PsN(modelFile = ctlFilename, seed = 123, ..., addargs = paste("--lst_file=", 
            paste(fileNameRoot, ".lst", sep = ""), " --bin_by_count=0 --bin_array=0.125,0.375,0.75,1.25,1.75,2.5,4.5,7.5,10.5,18,30,42,60,84,108,125 --dir=VPCdir", 
            sep = ""))
        cleanup(path = "VPCdir", remove.folders = T)
        
        #' Plotting the VPC using the xpose.VPC function in xpose.
        #' Call to xpose function xpose.VPC.
        if (length(outFormat) > 0) {
            if (outFormat == "png") 
                png(file = paste("Run", runno, "VPC", "%d.png", sep = ""))
            if (outFormat == "pdf") 
                pdf(file = paste("Run", runno, "VPC", "%d.pdf", sep = ""))
        }
        xpose.VPC(vpc.info = "./VPCdir/vpc_results.csv", vpctab = "./VPCdir/vpctab")
    }
    
    #' Bootstrap of the original model using PsN
    #' -------------------------
    #' Similarly to `VPC_PsN` here we can use the bootstrap functionality in PsN directly.
    if (bootstrap) {
        bootstrap_PsN(modelFile = ctlFilename, seed = 123, addarg = "--dir=BSdir", 
            ...)
        cleanup(path = "BSdir", remove.folders = T)
        
        #' Summarise bootstrap output i.e. pick out relevant numbers from raw_results...csv file.
        output$bootstrap <- bs.summary(fileName = "./BSdir/bootstrap_results.csv")
    }
    return(output)
    if (length(outFormat) > 0) 
        graphics.off()
}
