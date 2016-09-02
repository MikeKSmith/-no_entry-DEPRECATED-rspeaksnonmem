#' Finds the run number from sdtab, creates an xpdb and runs basic GOF plots
#'
#' @param runno NONMEM run number (based on sdtab).
#' @return Xpose core diagnostic plots
#' @examples
#' basicGOF_Xpose(1)

basicGOF_Xpose <- function(runno = NULL, ...) {
    ## ----setupRunnoforXpose--------------------------------------------------
    if (!length(runno) > 0) runno <- as.numeric(gsub("[a-z]", "", list.files(pattern = "^sdtab")[1]))
    
    ## ----createXpdb----------------------------------------------------------
    xpdb <- xpose4::xpose.data(runno, quiet = T)
    # save(base.xpdb, file='Xpose database.RData')
    
    ## ----xposeGOF------------------------------------------------------------
    print(xpose4::runsum(xpdb, ...))
    print(xpose4::dv.vs.pred.ipred(xpdb))
    print(xpose4::pred.vs.idv(xpdb))
    print(xpose4::ipred.vs.idv(xpdb))
    print(xpose4::wres.vs.idv(xpdb))
    print(xpose4::wres.vs.pred(xpdb))
    print(xpose4::ranpar.hist(xpdb))
    # print(parm.splom(base.xpdb)) print(parm.vs.cov(base.xpdb))
    print(xpose4::ind.plots(xpdb, layout = c(4, 4)))
    # etc. etc.
} 
