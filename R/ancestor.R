##' @method parent tbl_tree
##' @export
parent.tbl_tree <- function(.data, .node, ...) {
    x <- filter_(.data, ~ (node == .node | label == .node) & node != parent)
    if (nrow(x) == 0) ## root node
        return(x)
    filter_(.data, ~( node == x$parent))
}

##' @method ancestor tbl_tree
##' @export
ancestor.tbl_tree <- function(.data, .node, ...) {
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

##' @method mrca tbl_tree
##' @export
mrca.tbl_tree <- function(.data, .node1, .node2, ...) {
    anc1 <- ancestor(.data, .node1)
    if (nrow(anc1) == 0) {
        ## .node1 is root
        return(anc1)
    }
    p <- parent(.data, .node2)
    if (nrow(p) == 0) {
        ## .node2 is root
        return(p)
    }
    while(! p$node %in% anc1$node) {
        p <- parent(.data, p$node)
    }
    return(p)
}

##' @method root tbl_tree
##' @export
root.tbl_tree <- function(.data, ...) {
    filter_(.data, ~ parent == node)
}
