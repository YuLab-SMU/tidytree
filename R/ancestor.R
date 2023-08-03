##' @importFrom lazyeval interp
##' @method parent tbl_tree
##' @export
##' @rdname parent
##' @examples
##' library(ape)
##' tree <- rtree(4)
##' x <- as_tibble(tree)
##' parent(x, 2)
parent.tbl_tree <- function(.data, .node, ...) {
    valid.tbl_tree(.data)
    ## x <- filter_(.data, ~ (node == .node | label == .node) & node != parent)
    ## if (nrow(x) == 0) ## root node
    ##     return(x)
    ## ## https://stackoverflow.com/questions/34219912/how-to-use-a-variable-in-dplyrfilter
    ## filter_(.data, interp(~node == p, p = x$parent))

    ndata <- itself(.data, .node)
    .node <- ndata$node
    pnode <- ndata$parent

    if (pnode == .node)
        return(.data[0,]) ## empty tibble
    .data[.data$node == pnode, ]
}

itself <- function(.data, .node) {
    if (is.numeric(.node)) {
        i <- which(.data$node == .node)
    } else {
        i <- which(.data$label == .node)
    }
        
    ## .data[which(.data$node == .node | .data$label == .node), ]
    return(.data[i, ])
}

##' @method parent phylo
##' @export
parent.phylo <- function(.data, .node, ...) {
    vapply(.node, function(nn) {
        if ( nn == rootnode(.data) )
            return(0)
        edge <- .data[["edge"]]
        parent <- edge[,1]
        child <- edge[,2]
        res <- parent[child == nn]
        if (length(res) == 0) {
            stop("cannot found parent node...")
        }
        if (length(res) > 1) {
            stop("multiple parent found...")
        }
        return(res)
    }, numeric(1))
}

##' @method parent treedata
##' @export
parent.treedata <- function(.data,  .node,  ...) {
    parent.phylo(as.phylo(.data), .node, ...)
}



##' @method ancestor tbl_tree
##' @export
##' @rdname ancestor
##' @examples
##' library(ape)
##' tree <- rtree(4)
##' x <- as_tibble(tree)
##' ancestor(x, 3)
ancestor.tbl_tree <- function(.data, .node, ...) {
    ## prevent using filter
    ## see https://github.com/GuangchuangYu/tidytree/issues/4

    ndata <- itself(.data, .node)
    ## ndata <- filter_(.data, ~ (node == .node | label == .node))
    .node <- ndata$node
    pnode <- ndata$parent

    if (.node == pnode) {
        ## root node
        return(parent(.data, .node)) ## empty tibble
    }

    parent <- .data$parent
    children <- .data$node
    n <- length(children)

    pp <- vector("integer", n)
    for (i in seq_along(children)) {
        pp[[children[i]]] <- parent[i]
    }

    id <- pnode
    i <- 1
    while( i <= length(id) ) {
        pnode <- pp[id[i]]
        if (pnode == id[i])
            break
        id <- c(id, pnode)
        i <- i + 1
    }
    ## filter_(.data, ~ node %in% id)
    .data[children %in% id,]
}

##' @method ancestor phylo
##' @export
ancestor.phylo <- function(.data, .node, ...) {
    root <- rootnode(.data)
    if (.node == root) {
        return(NA)
    }
    p <- parent(.data, .node)
    res <- p
    while(p != root) {
        p <- parent(.data, p)
        res <- c(res, p)
    }
    return(res)
}

##' @method ancestor treedata
##' @export
ancestor.treedata <- function(.data, .node, ...) {
    ancestor.phylo(as.phylo(.data), .node, ...)
}

## ancestor.tbl_tree <- function(.data, .node, ...) {
##     p <- parent(.data, .node)
##     if (nrow(p) == 0)
##         return(p)
##     id <- p$node
##     i <- 1
##     while(i <= length(id)) {
##         p <- parent(.data, id[i])
##         if (nrow(p) == 0)
##             break
##         id <- c(id, p$node)
##         i <- i + 1
##     }
##     filter_(.data, ~ node %in% id)
## }


##' @method MRCA tbl_tree
##' @export
MRCA.tbl_tree <- function(.data, .node1, .node2 = NULL, ...) {
    if (length(.node1) == 1 && length(.node2) == 1) {
        return(.MRCA.tbl_tree_internal(.data, .node1, .node2, ...))
    } else if (is.null(.node2) && length(.node1) >= 1) {
        if (length(.node1) == 1) return(itself(.data, .node1))
        ## else length(.node1) > 1
        node <- .MRCA.tbl_tree_internal(.data, .node1[1], .node1[2])
        if (length(.node1) > 2) {
            for (i in 3:length(.node1)) {
                node <- .MRCA.tbl_tree_internal(.data, .node1[i], node$node)
            }
        }
        return(node)
    } else {
        stop("invalid input of '.node1' and '.node2'...")
    }
}

#' @noRd
#' @keywords internal 
.MRCA.tbl_tree_internal <- function(.data, .node1, .node2, ...) {
    anc1 <- ancestor(.data, .node1)
    if (nrow(anc1) == 0) {
        ## .node1 is root
        return(anc1)
    }
    if (.node2 %in% anc1$node) {
        ## .node2 is the ancestor of .node1
        return(filter(anc1, .data$node == .node2))
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
    ## filter_(.data, ~ parent == node)
    .data[.data$parent == .data$node, ]
}

##' @method rootnode phylo
##' @export
rootnode.phylo <- function(.data, ...) {
    edge <- .data[["edge"]]
    ## 1st col is parent,
    ## 2nd col is child,
    if (!is.null(attr(.data, "order")) && attr(.data, "order") == "postorder")
        return(edge[nrow(edge), 1])

    parent <- unique(edge[,1])
    child <- unique(edge[,2])
    ## the node that has no parent should be the root
    root <- parent[ ! parent %in% child ]
    if (length(root) > 1) {
        stop("multiple roots found...")
    }
    return(root)
}
