getCellPopulationCounts <- function(sce,
                                    patientVar,
                                    cellTypeVar,
                                    group,
                                    otherColData=NULL,
                                    format = "long"){
  # sce is a SCE object or data frame
  # patientVar is the variable in colData denoting patients
  # cellTypeVar is the variable in colData denoting cell types
  # group is the variable in colData denoting treatment covariate

  df <- data.frame(patient=colData(sce)[,patientVar],
                   celltype=colData(sce)[,cellTypeVar],
                   group=colData(sce)[,group])
  colDf <- as.data.frame(colData(sce)[,otherColData])
  colnames(colDf) <- otherColData
  df <- cbind(df,colDf)
  require(tidyverse)
  sumDf <- df %>%
    group_by(patient, celltype, group) %>%
    dplyr::summarize(nCells = n(),
                     across()) # across() keeps other column names

  if(format == "long"){
    return(sumDf)
  } else if(format == "wide"){
    ## transform to wide format
    popCounts <- pivot_wider(sumDf, names_from=celltype,
                             values_from=nCells,
                             values_fn = unique)
    popCounts[is.na(popCounts)] <- 0
    return(popCounts)
  } else {stop("format should be either 'long' or 'wide'.")}
}
