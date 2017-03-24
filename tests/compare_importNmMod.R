# library(rspeaksnonmem)
# library(RNMImport)
library(testthat)
devtools::load_all(pkg=".")
devtools::load_all(pkg = "C:\\Users\\smith_mk\\Documents\\Working documents\\RNMImport")

context("write NM control stream")

#' Check conversion of NONMEM example files
#' ----------------
files <- list.files(file.path("c:","nm72","examples"),pattern = ".ctl",full.names = T)
#' RNMImport does not support PRIOR or BAYES estimation (yet), so select only the "sde" examples
files1 <- files[grep("^sd",basename(files))]
#' Example sde9.ctl has single comment for both SIGMA parameters. TODO: Fix
files1 <- files1[-grep("^sde9", basename(files1))]

#' Gather example files
#' ----------------
#' Taking testdata examples from RNMImport, metrumrg, rspeaksnonmem.  

files2 <- list.files(file.path(system.file("unittests", package="RNMImport"),"testdata"),
                     pattern = "\\.ctl|\\.mod|\\.con",
                     full.names = T, recursive=T)

# multiprob1 example doesn't work due to $MSFI
files2 <- files2[-grep("multiprob1", basename(files2))]
#files2 <- files2[-grep("subprob1.mod", basename(files2))]
files2 <- files2[-grep("TestRun", dirname(files2))]

files3 <- list.files(file.path(system.file("example", package="metrumrg"),"project"),
                     pattern = "\\.ctl|\\.mod|\\.con",
                     full.names = T, recursive=T)

# 1005 & 1006 have issues in RNMImport around naming of parameters based on comments
files3 <- files3[-grep("1005",basename(files3))]
files3 <- files3[-grep("1006",basename(files3))]

files4 <- list.files("C:\\Users\\smith_mk\\Documents\\Working documents\\rspeaksnonmem\\inst\\exdata",
                     pattern = "\\.ctl|\\.mod|\\.con",
                     full.names = T, recursive=T)

#' Taking examples from DDMoRe SEE
files5 <- list.files("C:\\SEE\\MDL_IDE\\workspace\\UseCasesDemo\\",
                     pattern="\\.ctl", full.names=T, recursive=T)
files5 <- files5[!duplicated(basename(files5))]

input <- c(files1,files2, files3,files4)

sink("test_original_written.txt")

for (i in 1:length(input)){
  file <-input[i]
  fileTxt <- paste("\n", i, "File:", file,"")
  cat(fileTxt, sep="\n")
  ref <- RNMImport::importNmMod(file)
  try(writeNMControlStream(templateModel=ref$Raw, 
                           parsedControl = ref$problemContents,
                           outputFile = "temp",
                           outFileExtension = "ctl"))
  test <- RNMImport::importNmMod("temp.ctl")
  probPass <- NULL
  for (j in 1:length(ref$problemContents)){
    probName <- paste("Problem:",ref$problemContents[[j]]$Problem)
    cat(probName,sep="\n")
    sectionPass <- NULL
    sectionTestTxt <- NULL
    for (k in 1:length(ref$problemContents[[j]])){
      sectionPass[k] <- identical(ref$problemContents[[j]][k], test$problemContents[[j]][k])
      sectionTestTxt[k] <- paste("\t",names(ref$problemContents[[j]][k]),":",
                                 ifelse(sectionPass[k],"PASS","FAIL"))
      cat(sectionTestTxt[k], sep="\n")
      if (!sectionPass[k]){
        cat("\nref:")
        print(ref$problemContents[[j]][k])
        cat("\ntest:")
        print(test$problemContents[[j]][k])
      }
    }
    probPass[j] <- all(sectionPass)
  }
  testPass <- all(probPass)
  testPassTxt <- paste("All sections identical:",ifelse(testPass,"PASS","FAIL"))
  cat(testPassTxt, sep="\n")
}
sink()