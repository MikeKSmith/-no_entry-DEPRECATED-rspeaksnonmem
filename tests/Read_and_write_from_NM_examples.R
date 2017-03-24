#' ----------------
#' title: "Reading NMTRAN control streams using RNMImport and writing with rspeaksnonmem"
#' author: "Mike Smith "
#' date: "`r Sys.Date()`"
#' ----------------
#' Load packages  
#' ----------------
devtools::load_all(pkg=".")
devtools::load_all(pkg = "C:\\Users\\smith_mk\\Documents\\Working documents\\RNMImport")

#' Check conversion of NONMEM example files
#' ----------------
files <- list.files(file.path("c:","nm72","examples"),pattern = ".ctl",full.names = T)
#' RNMImport does not support PRIOR or BAYES estimation (yet), so select only the "sde" examples
files1 <- files[grep("^sd",basename(files))]

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

#' Copy files into a single directory
setwd(system.file("tests", package="rspeaksnonmem"))

#' Original files go in /originals folder
originals <- file.path(getwd(),"originals")
dir.create(originals)

#' control streams written by rspeaksnonmem go in the /written directory
written <- file.path(getwd(),"written")
dir.create(written)

lapply(files1, function(x)file.copy(from=x, to=originals))
lapply(files2, function(x)file.copy(from=x, to=originals))
lapply(files3, function(x)file.copy(from=x, to=originals))
lapply(files4, function(x)file.copy(from=x, to=originals))

input <- c(files1,files2, files3,files4)

#' Clean out any previously written files
unlink(list.files(path=file.path("written")))

#' Read and write
#' --------------------
#' The aim of this step is to test the writeNmControlStream function from rspeaksnonmem. 
#' If the resulting written NMTRAN control streams match content with the original then
#' the writeNmControlStream function is working as expected. Note though that because of 
#' the flexibility in writing NMTRAN, the two files may not match EXACTLY. This is to be
#' expected. We could perform a check that the models are equivalent formally by running 
#' the model with the original data and comparing OFV, or simulating new data first then 
#' performing estimation and comparing OFV. 
#'  
#' For now we need to check the output manually to assess whether the original and written
#' files are comparable.
#'  
inFiles <- lapply(input, function(x){
  cat(x,sep="\n")
  foo <- RNMImport::importNmMod(x)
  try(writeNMControlStream(templateModel=foo$Raw, 
                       parsedControl = foo$problemContents,
                       outputFile = file.path(written,tools::file_path_sans_ext(basename(x))),
                       outFileExtension = tools::file_ext(x)))
  })

