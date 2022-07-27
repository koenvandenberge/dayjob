getCellPopulationCounts <- function(sce,
                                    patientVar,
                                    cellTypeVar,
                                    group,
                                    format = "long"){
  # sce is a SCE object or data frame
  # patientVar is the variable in colData denoting patients
  # cellTypeVar is the variable in colData denoting cell types
  # group is the variable in colData denoting treatment covariate

  df <- data.frame(patient=colData(sce)[,patientVar],
                   celltype=colData(sce)[,cellTypeVar],
                   group=colData(sce)[,group])
  require(tidyverse)
  sumDf <- df %>%
    group_by(patient, celltype, group) %>%
    summarize(nCells = n())

  if(format == "long"){
    return(sumDf)
  } else if(format == "wide"){
    ## transform to wide format
    popCounts <- pivot_wider(sumDf, names_from=celltype, values_from=nCells)
    popCounts[is.na(popCounts)] <- 0
    return(popCounts)
  } else {stop("format should be either 'long' or 'wide'.")}
}
