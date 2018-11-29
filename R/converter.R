
##' @method as.phylo treedata
##' @export
as.phylo.treedata <- function(x, ...) {
    return(x@phylo)
}


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


##' @importFrom methods new
##' @method as.treedata tbl_tree
##' @export
##' @rdname as.treedata
##' @examples
##' library(ape)
##' set.seed(2017)
##' tree <- rtree(4)
##' d <- tibble(label = paste0('t', 1:4),
##'            trait = rnorm(4))
##' x <- as_tibble(tree)
##' full_join(x, d, by = 'label') %>% as.treedata
as.treedata.tbl_tree <- function(tree, ...) {
    data <- tree
    cn <- colnames(data)
    idx <- cn[!cn %in% c("parent", "branch.length", "label", "isTip", "x", "y", "branch", "angle")]
    res <- new("treedata",
               phylo = as.phylo.tbl_tree(data))
    if (length(idx))
        res@data <- as_tibble(data[, idx])
    return(res)
}
