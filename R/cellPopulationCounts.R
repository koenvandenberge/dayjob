getCellPopulationCounts <- function(sce,
                                    patientVar,
                                    cellTypeVar,
                                    group){
  # sce is a SCE object
  # patientVar is the variable in colData denoting patients
  # cellTypeVar is the variable in colData denoting cell types
  # group is the variable in colData denoting treatment covariate

  df <- data.frame(patient=sce$PATIENT_ID,
                   celltype=sce$CT_clean.label,
                   group=sce$COHORT)
  require(tidyverse)
  sumDf <- df %>%
    group_by(patient, celltype, group) %>%
    summarize(nCells = n())

  ## transform to wide format
  popCounts <- pivot_wider(sumDf, names_from=celltype, values_from=nCells)
  popCounts[is.na(popCounts)] <- 0
  return(popCounts)
}
