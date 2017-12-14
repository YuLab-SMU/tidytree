##' @importFrom dplyr mutate_
##' @importFrom ape as.phylo
##' @method as.phylo tbl_tree
##' @export
## original contributed by Bradley Jones and modified by Guangchuang Yu
as.phylo.tbl_tree <- function(x, ...) {
    valid.tbl_tree(x)

    edge <- x[, c("parent", "node")]
    i <- which(edge[,1] != 0 & edge[,1] != edge[,2])
    edge <- edge[i, ]
    if (is.null(x[["branch.length"]])) {
        edge.length <- NULL
    } else {
        edge.length <- x$branch.length[i]
    }

    x %<>% mutate_(isTip = ~(! node %in% parent))
    tip.label <- as.character(x$label[x$isTip])

    phylo <- list(edge = as.matrix(edge),
                  edge.length = edge.length,
                  tip.label = tip.label)

    node.label <- as.character(x$label[!x$isTip])
    if (!all(is.na(node.label))) {
        phylo$node.label <- node.label
    }
    phylo$Nnode <- sum(!x[, "isTip"])
    class(phylo) <- "phylo"
    return(phylo)
}

valid.tbl_tree <- function(object, cols = c("parent", "node", "label")) {
    cc <- cols[!cols %in% colnames(object)]
    if (length(cc) > 0) {
        msg <- paste0("invalid tbl_tree object.\n  missing column:\n    ", paste(cc, collapse=","), ".")
    }
}

##' @importFrom methods new
##' @method as.treedata tbl_tree
##' @export
as.treedata.tbl_tree <- function(tree, ...) {
    data <- tree
    cn <- colnames(data)
    idx <- cn[!cn %in% c("parent", "branch.length", "label", "isTip", "x", "y", "branch", "angle")]
    res <- new("treedata",
               phylo = as.phylo.tbl_tree(data))
    if (length(idx))
        res@data <- as_data_frame(data[, idx])
    return(res)
}
