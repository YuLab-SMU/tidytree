##' @method child tbl_tree
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
    if (missing(.node) || is.null(.node)) {
        stop(".node is required")
    }
    if (length(.node) == 1) {
        res <- .offspring.tbl_tree_item(.data = .data, .node = .node,
                                       tiponly = tiponly, self_include = self_include, ...)
    } else {
        res <- lapply(.node, function(node) {
            .offspring.tbl_tree_item(.data = .data, .node = node,
                                    tiponly = tiponly, self_include = self_include, ...)
        })
        names(res) <- .node
    }
    return(res)
}

#' @noRd
#' @keywords internal
.offspring.tbl_tree_item <- function(.data, .node, tiponly = FALSE, self_include = FALSE, ...) {
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

##' @method child phylo
##' @export
child.phylo <- function(.data, .node, type = 'children', ...) {
    res <- offspring(.data=.data, .node = .node, type = type)
    return(res)
}

##' @method child treedata
##' @export
child.treedata <- function(.data, .node, type = 'children', ...) {
    child.phylo(as.phylo(.data), .node, type = type, ...)
}

.internal.child <- function(data, node, type = 'children'){
    if (!is_numeric(node)){
        all.labs <- c(data$tip.label, data$node.label)
        names(all.labs) <- seq_len(length(all.labs))
        node <- names(all.labs[all.labs %in% node])
    }
    edge <- data$edge
    res <- edge[edge[,1] == node, 2]
    if (type != 'children'){
        alltips <- edge[,2][! edge[,2] %in% edge[,1]]
        w <- which(res >= length(alltips))
        if(length(w)>0){
            for(i in 1:length(w)){
                res <- c(res,
                         .internal.child(
                           data = data,
                           node = res[w[i]],
                           type = type
                         )
                       )
            }
        }
        if (type %in% c('tips', 'external')){
            res <- res[res %in% alltips]
        }else if (type == "internal") {
            res <- res[!res %in% alltips]
        }
    }
    return(unname(res))
}

##' @method offspring phylo
##' @export
offspring.phylo <- function(.data, .node, tiponly = FALSE, self_include = FALSE, type = 'all', ...){
    type <- match.arg(type, c("children", 'tips', 'internal', 'external', 'all'))

    if (tiponly){
        message('The "tiponly = TRUE" can be replaced by type="tips".')
        type = 'tips'
    }

    res <- lapply(.node, .internal.child, data = .data, type = type)
    if (length(res) <= 1){
        res <- unlist(res)
        if (self_include){
            res <- c(.node, res)
        }
    }else{
        if (self_include){
            res <- mapply(append, .node, res, SIMPLIFY=FALSE)
        }
        names(res) <- .node
    }
    return (res)
    #if (self_include) {
    #    sp <- .node
    #} else {
    #    sp <- child(.data, .node)
    #}

    #sp <- sp[sp != 0]
    #if (length(sp) == 0) {
    #    return(sp)
    #    ## stop("input node is a tip...")
    #}
    #i <- 1
    #while (i <= length(sp)) {
    #    sp <- c(sp, child(.data, sp[i]))
    #    sp <- sp[sp != 0]
    #    i <- i + 1
    #}
    #if (tiponly) {
    #    return(sp[sp <= Ntip(.data)])
    #}
    #return(sp)
}


##' @method offspring treedata
##' @export
offspring.treedata <- function(.data, .node, tiponly = FALSE, self_include = FALSE, type = 'all', ...) {
    offspring.phylo(as.phylo(.data), .node,
                    tiponly = tiponly, self_include = self_include,
                    type = type,
                    ...)
}
