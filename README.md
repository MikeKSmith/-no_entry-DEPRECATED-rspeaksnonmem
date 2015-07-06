# rspeaksnonmem
R package for working with and executing NONMEM control files, also integrating PsN

## Description

`rspeaksnonmem` is designed to allow the user to craft workflows based on a given NONMEM model.

After importing and parsing a control stream to an R object (using the `RNMImport` package), `rspeaksnonmem` allows the user to easily change initial estimates, change data attributes or change task properties (estimation or simulation settings) without having to change the model. This saves a lot of textual searching by the user. The resulting data, parameter,
model and task information is then written back out to file using an existing (possibly the original) model as a 
template.

`rspeaksnonmem` also allows the user to run NONMEM models from R either directly running the nmfe.bat or by
calling [Perl speaks NONMEM](http://psn.sourceforge.net) functions like "execute". Other PsN functions can be run from
R - VPC (Visual Predictive Check), bootstrap, SSE (Stochastic Simulation and Estimation).

## Installation

Eventually, `rspeaksnonmem` will be released to CRAN, but while still in development `rspeaksnonmem` can most easily be installed from GitHub using the `devtools` package:

    library(devtools)
    install_github("MikeKSmith/rspeaksnonmem")

## Dependencies

`rspeaksnonmem` relies on the package [`RNMImport`](https://r-forge.r-project.org/R/?group_id=1922) function 
`importNmMod` which reads and parses the NONMEM control stream. `rspeaksnonmem` then works with
the data, parameter values, and task information separately from the model.

Note that before installation, the package `RNMImport` needs to be installed (not on CRAN, so manual installation). Download the `.tar` file from [R-Forge](https://r-forge.r-project.org/R/?group_id=1922) and run in R:

    install.packages("~/Downloads/RNMImport_4.0-27.tar.gz", repos=NULL,type = "source")

