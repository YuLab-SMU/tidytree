##' @method child tbl_tree
##' @importFrom dplyr filter_
##' @export
##' @rdname child
##' @examples
##' library(ape)
##' tree <- rtree(4)
##' x <- as_tibble(tree)
##' child(x, 4)
child.tbl_tree <- function(.data, .node, ...) {
    valid.tbl_tree(.data)

    if (is.character(.node)) {
        .node <- .data$node[.data$label == .node]
    }

    .data[.data$parent == .node & .data$parent != .data$node,]
}

##' @method offspring tbl_tree
##' @export
##' @rdname offspring
##' @examples
##' library(ape)
##' tree <- rtree(4)
##' x <- as_tibble(tree)
##' offspring(x, 4)
offspring.tbl_tree <- function(.data, .node, tiponly = FALSE, self_include = FALSE, ...) {
    x <- child.tbl_tree(.data, .node)

    ## https://github.com/GuangchuangYu/ggtree/issues/239
    rn <- rootnode.tbl_tree(.data)$node
    x <- x[x$node != rn, ]

    if (nrow(x) == 0) {
        if (self_include) {
            x <- .data[.data$node == .node, ]
        } 

        return(x)
    }

    ## id <- x$node
    ## i <- 1
    ## while(i <= length(id)) {
    ##     id <- c(id, child(.data, id[i])$node)
    ##     i <- i + 1
    ## }
    ## filter_(.data, ~ node %in% id)

    parent <- .data$parent
    children <- .data$node
    ## n <- length(parent)
    n <- max(parent)

    kids <- vector("list", n)
    for (i in seq_along(parent)) {
        kids[[parent[i]]] <-c(kids[[parent[i]]], children[i])
    }

    id <- x$node
    i <- 1
    while(i <= length(id)) {
        id <- c(id, kids[[id[i]]])
        i <- i + 1
    }

    if (self_include) {
        id <- c(.node, id)
    }

    sp <- .data[children %in% id,]
    if (tiponly) {
        return(sp[sp$node < rn,])
    }
    return(sp)
}

