##' @method nodeid tbl_tree
##' @export
nodeid.tbl_tree <- function(tree, label) {
    tree$node[match(label, tree$label)]
}

##' @method nodelab tbl_tree
##' @export
nodelab.tbl_tree <- function(tree, id) {
    tree$label[match(id, tree$id)]
}

