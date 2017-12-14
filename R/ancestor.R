##' @importFrom lazyeval interp
##' @method parent tbl_tree
##' @export
##' @rdname parent
##' @examples
##' library(ape)
##' tree <- rtree(4)
##' x <- as_data_frame(tree)
##' parent(x, 2)
parent.tbl_tree <- function(.data, .node, ...) {
    valid.tbl_tree(.data)
    x <- filter_(.data, ~ (node == .node | label == .node) & node != parent)
    if (nrow(x) == 0) ## root node
        return(x)
    ## https://stackoverflow.com/questions/34219912/how-to-use-a-variable-in-dplyrfilter
    filter_(.data, interp(~node == p, p = x$parent))
}

##' @method ancestor tbl_tree
##' @export
##' @rdname ancestor
##' @examples
##' library(ape)
##' tree <- rtree(4)
##' x <- as_data_frame(tree)
##' ancestor(x, 3)
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

##' @method MRCA tbl_tree
##' @export
MRCA.tbl_tree <- function(.data, .node1, .node2, ...) {
    anc1 <- ancestor(.data, .node1)
    if (nrow(anc1) == 0) {
        ## .node1 is root
        return(anc1)
    }
    if (.node2 %in% anc1$node) {
        ## .node2 is the ancestor of .node1
        return(filter_(anc1, ~ node == .node2))
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

##' @method rootnode tbl_tree
##' @export
rootnode.tbl_tree <- function(.data, ...) {
    valid.tbl_tree(.data)
    filter_(.data, ~ parent == node)
}
