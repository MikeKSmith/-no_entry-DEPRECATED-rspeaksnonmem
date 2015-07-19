#' Finds the run number from sdtab, creates an xpdb and runs basic GOF plots
#'
#' @param runno NONMEM run number (based on sdtab).
#' @return Xpose core diagnostic plots
#' @examples
#' basicGOF.Xpose(1)

basicGOF.Xpose <- function(runno = NULL) {
    ## ----setupRunnoforXpose--------------------------------------------------
    if (!length(runno) > 0) 
        runno <- as.numeric(gsub("[a-z]", "", list.files(pattern = "^sdtab")[1]))
    
    
    ## ----createXpdb----------------------------------------------------------
    base.xpdb <- xpose4::xpose.data(runno)
    # save(base.xpdb, file='Xpose database.RData')
    
    ## ----xposeGOF------------------------------------------------------------
    print(xpose4::dv.vs.pred.ipred(base.xpdb))
    print(xpose4::pred.vs.idv(base.xpdb))
    print(xpose4::ipred.vs.idv(base.xpdb))
    print(xpose4::wres.vs.idv(base.xpdb))
    print(xpose4::wres.vs.pred(base.xpdb))
    print(xpose4::ranpar.hist(base.xpdb))
    # print(parm.splom(base.xpdb)) print(parm.vs.cov(base.xpdb))
    print(xpose4::ind.plots(base.xpdb, layout = c(4, 4)))
    # etc. etc.
} 
