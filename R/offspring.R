##' @method child tree_tbl
##' @importFrom dplyr filter_
##' @export
child.tree_tbl <- function(.data, .node, ...) {
    filter_(.data, ~(parent == .node | label == .node) & node != parent)
}

##' @method offspring tree_tbl
##' @export
offspring.tree_tbl <- function(.data, .node, ...) {
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

