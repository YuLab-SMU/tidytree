#' @method nodeid tbl_tree
#' @export
nodeid.tbl_tree <- function(tree, label) {
    tree$node[match(label, tree$label)]
}

#' @method nodelab tbl_tree
#' @export
nodelab.tbl_tree <- function(tree, id) {
    tree$label[match(id, tree$node)]
}

#' @method nodeid phylo
#' @export
nodeid.phylo <- function(tree, label) {
    ## nodeid(as_tibble(tree), label)
    lab <- c(tree$tip.label, tree$node.label)
    match(label, lab)
}

#' @method nodeid treedata
#' @export
nodeid.treedata <- function(tree, label) {
    nodeid(as.phylo(tree), label)
}

#' @method nodelab phylo
#' @export
nodelab.phylo <- function(tree, id) {
    nodelab(as_tibble(tree), id)
}

#' @method nodelab treedata
#' @export
nodelab.treedata <- function(tree, id) {
    nodelab(as.phylo(tree), id)
}
