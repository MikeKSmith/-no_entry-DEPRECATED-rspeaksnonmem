PsNHeader <- function(problem = NULL,
                      parentRun = NULL, 
                      description = NULL, 
                      runLabel = NULL, 
                      structure = NULL, 
                      covariates = NULL, 
                      IIV = NULL, 
                      IOV = NULL, 
                      RUV = NULL, 
                      estimation = NULL) {

  paste(problem, "\n", 
        ";; 1. Based on:", parentRun, "\n", 
        ";; 2. Description:\n", ";; ", description, "\n", 
        ";; 3. Label:\n", ";; ", runLabel, "\n", 
        ";; 4. Structural model:\n", ";; ", structure, "\n", 
        ";; 5. Covariate model:\n", ";; ", covariates, "\n",
        ";; 6. Inter-individual variability:\n", ";; ", IIV, "\n", 
        ";; 7. Inter-occasion variability:\n", ";; ", IOV, "\n", 
        ";; 8. Residual variability:\n", ";; ", RUV, "\n", 
        ";; 9. Estimation:\n", ";; ", estimation, sep = "")
  
}
