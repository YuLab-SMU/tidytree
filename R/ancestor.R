##' @method parent tree_tbl
##' @export
parent.tree_tbl <- function(.data, .node, ...) {
    x <- filter_(.data, ~ (node == .node | label == .node) & node != parent)
    if (nrow(x) == 0) ## root node
        return(x)
    filter_(.data, ~( node == x$parent))
}

##' @method ancestor tree_tbl
##' @export
ancestor.tree_tbl <- function(.data, .node, ...) {
    p <- parent(.data, .node)
    if (nrow(p) == 0)
        return(p)
    id <- p$node
    i <- 1
    while(i <= length(id)) {
        p <- parent(.data, id[i])
        if (nrow(p) == 0)
            break
        id <- c(id, p$node)
        i <- i + 1
    }
    filter_(.data, ~ node %in% id)
}
