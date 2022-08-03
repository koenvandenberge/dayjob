gsea <- function(genes, background, geneSets, n=10, minSize=5, name=NULL){
  ### filter background to only include genes that we assessed.
  geneSets <- geneSets[geneSets$gene_symbol %in% background,]
  m_list <- geneSets %>% split(x = .$gene_symbol, f = .$gs_name)
  # gene set must have at least minSize genes in background.
  m_list <- m_list[unlist(lapply(m_list, length)) >= minSize]

  overlapPval <- unlist(lapply(m_list, function(gs){
    # genes in community and gene set
    inBoth <- sum(genes %in% gs)
    # genes in community and not in gene set
    inComOnly <- length(genes) - inBoth
    # genes in background and gene set
    inGsBack <- sum(background %in% gs)
    # genes in background and not in gene set
    outGsBack <- length(background) - inGsBack
    m <- matrix(c(inBoth, inComOnly,
                  inGsBack, outGsBack),
                nrow =2, ncol=2, byrow=TRUE,
                dimnames = list(c("in community", "out community"),
                                c("in gene set", "out gene set")))
    fis <- fisher.test(m, alternative = "greater")
    pval <- fis$p.value
    return(pval)
  }))
  padj <- p.adjust(overlapPval, "fdr")
  oo <- order(overlapPval, decreasing=FALSE)
  res <- data.frame(geneSet = names(m_list)[oo[1:n]],
                    pval = overlapPval[oo[1:n]],
                    padj = padj[oo[1:n]],
                    row.names = NULL)
  kable(res, caption=name, label=name)
}
