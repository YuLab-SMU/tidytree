.onAttach <- function(...){
  ref <- random_ref()
  if (!is.null(ref)) packageStartupMessage(ref)
}

#' @importFrom pillar style_subtle
random_ref <- function(pkgname = NULL, pkgVersion = NULL, random_n = 2){
  if (!is.null(pkgname) && !is.null(pkgVersion)){
    headermsg <- paste0(pkgname, " v", pkgVersion, "  ",
                        "For help: https://yulab-smu.top/treedata-book/", "\n\n")
  }else{
    headermsg <- NULL
  }
  msg <- "If you use the ggtree package suite in published research, please cite the appropriate paper(s):\n\n" 
  refs <- c(
    ggtreeBook = paste("Guangchuang Yu. (2022).",
                        "Data Integration, Manipulation and Visualization of Phylogenetic Trees (1st edition).",
                        "Chapman and Hall/CRC. doi:10.1201/9781003279242\n"),
    ggtreeCPB = paste0(
      "Guangchuang Yu. ",
      "Using ggtree to visualize data on tree-like structures. ",
      "Current Protocols in Bioinformatics. 2020, 69:e96. doi:10.1002/cpbi.96\n"
    ),
    ggtreeMBE = paste0(
      "Guangchuang Yu, Tommy Tsan-Yuk Lam, Huachen Zhu, Yi Guan. ",
      "Two methods for mapping and visualizing associated data on phylogeny using ggtree. ",
      "Molecular Biology and Evolution. 2018, 35(12):3041-3043. doi:10.1093/molbev/msy194\n"
    ),
    ggtree = paste0(
      "Guangchuang Yu, David Smith, Huachen Zhu, Yi Guan, Tommy Tsan-Yuk Lam. ",
      "ggtree: an R package for visualization and annotation of phylogenetic trees with their covariates and other associated data. ",
      "Methods in Ecology and Evolution. 2017, 8(1):28-36. doi:10.1111/2041-210X.12628\n"
    ),
    treeio = paste0(
      "LG Wang, TTY Lam, S Xu, Z Dai, L Zhou, T Feng, P Guo, CW Dunn, BR Jones, T Bradley, H Zhu, Y Guan, Y Jiang, G Yu. ",
      "treeio: an R package for phylogenetic tree input and output with richly annotated and associated data. ",
      "Molecular Biology and Evolution. 2020, 37(2):599-603. doi: 10.1093/molbev/msz240\n"            
    ),
    ggtreeExtra = paste0(
      "S Xu, Z Dai, P Guo, X Fu, S Liu, L Zhou, W Tang, T Feng, M Chen, L Zhan, T Wu, E Hu, Y Jiang, X Bo, G Yu. ",
      "ggtreeExtra: Compact visualization of richly annotated phylogenetic data. ",
      "Molecular Biology and Evolution. 2021, 38(9):4039-4042. doi: 10.1093/molbev/msab166\n"
    ),
    ggtreeCRC = paste0(
      "G Yu. ", 
      "Data Integration, Manipulation and Visualization of Phylogenetic Trees (1st ed.). ", 
      "Chapman and Hall/CRC. 2022. ISBN: 9781032233574\n"
    )
  )
  if (is.null(pkgname)){
      refs <- paste0(sample(refs, random_n), collapse="\n")
  }else{
      indx <- match(pkgname, names(refs))
      refs <- paste0(c(refs[indx], sample(refs[-indx], random_n)), collapse="\n")
  }
  if (all(nchar(refs) > 0)){
      return(paste(strwrap(style_subtle(paste0(headermsg, msg, refs))), collapse = "\n"))
  }else{
      return(NULL)
  }
}
