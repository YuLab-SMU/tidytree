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
    ## filter_(.data, ~(parent == .node | label == .node) & node != parent)

    ndata <- .data[which(.data$node == .node | .data$label == .node), ]
    .node <- ndata$node

    i <- which(.data$parent == .node & .data$parent != .data$node)
    if (length(i) == 0) {
        ## tip
        return(.data[0,])
    }
    .data[i,]
}

##' @method offspring tbl_tree
##' @export
##' @rdname offspring
##' @examples
##' library(ape)
##' tree <- rtree(4)
##' x <- as_tibble(tree)
##' offspring(x, 4)
offspring.tbl_tree <- function(.data, .node, ...) {
    x <- child(.data, .node)
    if (nrow(x) == 0)
        return(x)

    ## id <- x$node
    ## i <- 1
    ## while(i <= length(id)) {
    ##     id <- c(id, child(.data, id[i])$node)
    ##     i <- i + 1
    ## }
    ## filter_(.data, ~ node %in% id)

    parent <- .data$parent
    children <- .data$node
    n <- length(parent)

    kids <- vector("list", n)
    for (i in 1:n) {
        kids[[parent[i]]] <-c(kids[[parent[i]]], children[i])
    }

    id <- x$node
    i <- 1
    while(i <= length(id)) {
        id <- c(id, kids[[id[i]]])
        i <- i + 1
    }

    .data[children %in% id,]
}

