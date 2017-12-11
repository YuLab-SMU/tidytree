##' @importFrom treeio child
##' @method child tbl_tree
##' @importFrom dplyr filter_
##' @export
child.tbl_tree <- function(.data, .node, ...) {
    filter_(.data, ~(parent == .node | label == .node) & node != parent)
}

##' @importFrom treeio offspring
##' @method offspring tbl_tree
##' @export
offspring.tbl_tree <- function(.data, .node, ...) {
    x <- child(.data, .node)
    if (nrow(x) == 0)
        return(x)

    id <- x$node
    i <- 1
    while(i <= length(id)) {
        id <- c(id, child(.data, id[i])$node)
        i <- i + 1
    }
    filter_(.data, ~ node %in% id)
}

