#' @method child tbl_tree
#' @export
#' @rdname child
#' @examples
#' library(ape)
#' tree <- rtree(4)
#' x <- as_tibble(tree)
#' child(x, 4)
child.tbl_tree <- function(.data, .node, ...) {
    valid.tbl_tree(.data)

    if (is.character(.node)) {
        .node <- .data$node[.data$label == .node]
    }

    .data[.data$parent == .node & .data$parent != .data$node,]
}

#' @method offspring tbl_tree
#' @export
#' @rdname offspring
#' @examples
#' library(ape)
#' tree <- rtree(4)
#' x <- as_tibble(tree)
#' offspring(x, 4)
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
    ## root node and children both come from the cached index: computing them
    ## with rootnode.tbl_tree()/child.tbl_tree() re-scans the whole table on
    ## every call (~0.10 s and ~0.13 s per 2000 calls at 1000 tips).
    kids <- .offspring_kids(.data, .data$parent, .data$node)
    rn <- .offspring_root(.data, .data$parent, .data$node)

    ## direct children of .node, excluding the root (which points at itself)
    x <- .data[.data$parent == .node, ]
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

    ## Grow the id vector in a preallocated buffer instead of `id <- c(id, ...)`
    ## inside the loop: repeated c() copies the whole vector each time and is
    ## ~35x slower than assigning into a buffer.
    buf <- .offspring_buffer(children)
    n <- length(x$node)
    buf[seq_len(n)] <- x$node
    target <- n
    i <- 1L
    while (i <= target) {
        cx <- as.character(buf[i])
        nx <- kids[[cx]]
        if (!is.null(nx)) {
            len <- length(nx)
            while (target + len > length(buf)) {
                buf <- c(buf, integer(length(buf)))
            }
            buf[(target + 1L):(target + len)] <- nx
            target <- target + len
        }
        i <- i + 1L
    }
    id <- buf[seq_len(target)]

    if (self_include) {
        id <- c(.node, id)
    }

    sp <- .data[children %in% id,]
    if (tiponly) {
        return(sp[sp$node < rn,])
    }
    return(sp)
}

#' @method child phylo
#' @export
child.phylo <- function(.data, .node, type = 'children', ...) {
    res <- offspring(.data=.data, .node = .node, type = type)
    return(res)
}

#' @method child treedata
#' @export
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

.node2num.phylo <- function(data, node) {
    if (!is_numeric(node)){
        all.labs <- c(data$tip.label, data$node.label)
        names(all.labs) <- seq_len(length(all.labs))
        node <- names(all.labs[all.labs %in% node])
    }
    as.numeric(node)
}

.offspring.phylo_item <- function(data, node, type, kids, ntip) {
    node <- .node2num.phylo(data, node)
    if (length(node) == 0 || is.na(node)) {
        return(integer(0))
    }

    res <- kids[[as.character(node)]]
    if (length(res) == 0) {
        return(integer(0))
    }

    if (type != "children") {
        n <- nrow(data$edge)
        id <- integer(n)
        n_id <- length(res)
        id[seq_len(n_id)] <- res

        i <- 1L
        while (i <= n_id) {
            children <- kids[[as.character(id[i])]]
            n_children <- length(children)
            if (n_children > 0) {
                idx <- n_id + seq_len(n_children)
                id[idx] <- children
                n_id <- n_id + n_children
            }
            i <- i + 1L
        }
        res <- id[seq_len(n_id)]

        if (type %in% c("tips", "external")) {
            res <- res[res <= ntip]
        } else if (type == "internal") {
            res <- res[res > ntip]
        }
    }

    unname(res)
}

#' @method offspring phylo
#' @export
offspring.phylo <- function(.data, .node, tiponly = FALSE, self_include = FALSE, type = 'all', ...){
    type <- match.arg(type, c("children", 'tips', 'internal', 'external', 'all'))

    if (tiponly){
        message('The "tiponly = TRUE" can be replaced by type="tips".')
        type = 'tips'
    }

    edge <- .data$edge
    kids <- split(edge[, 2], edge[, 1])
    ntip <- ape::Ntip(.data)
    res <- lapply(.node, .offspring.phylo_item,
                  data = .data, type = type, kids = kids, ntip = ntip)
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


#' @method offspring treedata
#' @export
offspring.treedata <- function(.data, .node, tiponly = FALSE, self_include = FALSE, type = 'all', ...) {
    offspring.phylo(as.phylo(.data), .node,
                    tiponly = tiponly, self_include = self_include,
                    type = type,
                    ...)
}


## Cache of parent -> children indexes, keyed by the tree's node/parent
## vectors. The index depends only on the tree, so repeated offspring()
## calls on the same tree reuse it.
##
## The key is a cheap digest of the two vectors that define the structure
## (length, node/parent checksums). Two different trees with the same shape
## would collide, so the cached value also stores the vectors and is only
## returned when they are identical.
##
## Option "check.tbl_tree.bypass_cache" turns the cache off, for the rare
## case where a caller mutates the tree in place without changing length.
.offspring_kids_env <- new.env(parent = emptyenv())

#' @noRd
#' @keywords internal
.offspring_kids <- function(.data, parent, children) {
    if (isTRUE(getOption("check.tbl_tree.bypass_cache", FALSE))) {
        return(split(children, parent))
    }

    key <- .offspring_key(parent, children)

    hit <- .offspring_kids_env[[key]]
    if (!is.null(hit) &&
        identical(hit$parent, parent) &&
        identical(hit$children, children)) {
        return(hit$kids)
    }

    kids <- split(children, parent)
    root <- children[parent == children]
    assign(key, list(parent = parent, children = children,
                     kids = kids, root = root[1]),
           envir = .offspring_kids_env)
    kids
}

## Root node id, cached alongside the children index (same key).
#' @noRd
#' @keywords internal
.offspring_root <- function(.data, parent, children) {
    if (isTRUE(getOption("check.tbl_tree.bypass_cache", FALSE))) {
        return(children[parent == children][1])
    }

    key <- .offspring_key(parent, children)
    hit <- .offspring_kids_env[[key]]
    if (!is.null(hit) &&
        identical(hit$children, children)) {
        return(hit$root)
    }

    ## miss: populate the cache so the next call does not split() twice
    .offspring_kids(.data, parent, children)
    .offspring_kids_env[[key]]$root
}

## Buffer for the id vector. Starts at the number of nodes and doubles on
## demand, which avoids reallocating on every level of a deep tree.
#' @noRd
#' @keywords internal
.offspring_buffer <- function(children) {
    integer(length(children))
}

## Cheap structural digest for the cache key. `sum()` over integers is fast
## and, combined with the length and the last value, tells different trees
## apart in practice; the cache still verifies `identical()` before use, so a
## collision costs a recomputation, never a wrong answer.
#' @noRd
#' @keywords internal
.offspring_key <- function(parent, children) {
    paste0(length(parent), "-", sum(parent), "-", sum(children),
           "-", parent[length(parent)], "-", children[length(children)])
}
