PsNHeader <- function(parsedObject, parentRun = NULL, comment = NULL, label = NULL, structure = NULL, covariates = NULL, IIV = NULL, IOV = NULL, RUV = NULL, 
    estimation = NULL) {
    problem <- paste(";;1. Based on:", parentRun, "\n", ";; 2. Description:\n", ";; ", comment, "\n", ";; 3. Label:\n", ";; ", label, "\n", ";; 4. Structural model:\n", 
        ";; ", structure, "\n", ";; 5. Covariate model:\n", ";; ", covariates, "\n", ";; 6. Inter-individual variability:\n", ";; ", IIV, "\n", ";; 7. Inter-occasion variability:\n", 
        ";; ", IOV, "\n", ";; 8. Residual variability:\n", ";; ", RUV, "\n", ";; 9. Estimation:\n", ";; ", estimation, sep = "")
    return(problem)
} 
