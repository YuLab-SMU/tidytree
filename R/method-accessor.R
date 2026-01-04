#' @title extract the node label of phylo, treedata or tbl_tree
#' @param x object, should be one of `treedata`, `phylo` or `tbl_tree`.
#' @param node character, to extract which type node label,
#' default is `internal`, should be one of `internal`,
#' `external`, `all`, `tip`.
#' @param ... additional parameters.
#' @return label character vector.
#' @export
node.label <- function(x, node='internal', ...){
    UseMethod("node.label")
}

#' @method node.label tbl_tree
#' @export
node.label.tbl_tree <- function(x, node = 'internal', ...){
    node <- match.arg(node, c("internal", "external", "all", "tip"))
    isTip <- .isTip.tbl_tree(x)
    if (node %in% c('external', 'tip')){
        lab <- x[isTip, "label", drop=TRUE]
    }else if (node == 'internal'){
        lab <- x[!isTip, 'label', drop=TRUE]
        if (all(is.na(lab))){
            lab <- NULL
        }
    }else if (node == 'all'){
        lab1 <- x[isTip, "label", drop=TRUE]
        lab2 <- x[!isTip, 'label', drop=TRUE]
        if (all(is.na(lab2))){
            lab2 <- NULL
        }
        lab <- c(lab1, lab2)
    }
    return(lab)
}

#' @method node.label phylo
#' @export
node.label.phylo <- function(x, node='internal',...){
    node <- match.arg(node, c("internal", "external", "all", "tip"))
    if (node %in% c("external", "tip")){
        lab <- x$tip.label
    }else if (node == 'internal'){
        lab <- x$node.label
    }else if (node == 'all'){
        lab <- c(x$tip.label, x$node.label)
    }
    return(lab)
}

#' @method node.label treedata
#' @export
node.label.treedata <- function(x, node = 'internal', ...){
    node.label(x@phylo, node = node, ...)
}

#' @title extract the tip label of phylo treedata or tbl_tree
#' @param x object, should be one of `treedata`, `phylo` or `tbl_tree`.
#' @param ... additional parameters.
#' @export
tip.label <- function(x, ...){
    UseMethod("tip.label")   
}

#' @method tip.label tbl_tree
#' @export
tip.label.tbl_tree <- function(x, ...){
    node.label(x, node = 'tip')
}

#' @method tip.label phylo
#' @export
tip.label.phylo <- function(x, ...){
    node.label(x, node = 'tip')
}

#' @method tip.label treedata
#' @export
tip.label.treedata <- function(x, ...){
    node.label(x, node = 'tip')
}

#' the tip or internal node label assign of tbl_tree phylo and treedata
#' @param x object, should be one of `tbl_tree`, `phylo` or `treedata`
#' @param value character, the character vector
#' @name td-label-assign
NULL

#' @rdname td-label-assign
#' @export
`tip.label<-` <- function(x, value){
    UseMethod('tip.label<-')
}

#' @rdname td-label-assign
#' @export
`node.label<-` <- function(x, value){
    UseMethod('node.label<-')
}

#' @method node.label<- phylo
#' @rdname td-label-assign
#' @export
`node.label<-.phylo` <- function(x, value){
    if (check.lab(value, ape::Nnode(x))){
        x$node.label <- as.character(value)
    }else{
        .internal.assign.lab.abort(x = 'internal node')
    }
    return(x)
}

#' @method node.label<- treedata
#' @rdname td-label-assign
#' @export
`node.label<-.treedata` <- function(x, value){
    node.label(x@phylo) <- value
    return(x)
}

#' @method node.label<- tbl_tree
#' @rdname td-label-assign
#' @export
`node.label<-.tbl_tree` <- function(x, value){
    isTip <- .isTip.tbl_tree(x)
    if (check.lab(value, Nnode(x))){
        x[!isTip, 'label',drop=TRUE] <- as.character(value)
    }else{
        .internal.assign.lab.abort(x = 'internal node')
    }
    return(x)
}

#' @method tip.label<- phylo
#' @rdname td-label-assign
#' @export
`tip.label<-.phylo` <- function(x, value){
    if (check.lab(value, ape::Ntip(x))){
        x$tip.label <- as.character(value)
    }else{
        .internal.assign.lab.abort(x = 'tip node')
    }
    return(x)
}

#' @method tip.label<- treedata
#' @rdname td-label-assign
#' @export
`tip.label<-.treedata` <- function(x, value){
    tip.label(x@phylo) <- value
    return(x)
}

#' @method tip.label<- tbl_tree
#' @rdname td-label-assign
#' @export
`tip.label<-.tbl_tree` <- function(x, value){
    isTip <- .isTip.tbl_tree(x)
    if (check.lab(value, Ntip(x))){
        x[isTip,'label',drop=TRUE] <- as.character(value)
    }else{
        .internal.assign.lab.abort(x = 'tip node')
    }
    return(x)
}

#' @method Ntip tbl_tree 
#' @importFrom ape Ntip
#' @export
Ntip.tbl_tree <- function(phy){
    sum(!phy[,2,drop=TRUE] %in% phy[,1,drop=TRUE])
}

#' @method Nnode tbl_tree
#' @importFrom ape Nnode
#' @export
Nnode.tbl_tree <- function(phy, internal.only = TRUE, ...){
    if (!internal.only){
        return(nrow(phy))
    }
    nrow(phy) - Ntip(phy)
}

#' @noRd
.isTip.tbl_tree <- function(x){
    !x[,2,drop=TRUE] %in% x[,1,drop=TRUE]
}

check.lab <- function(x, y){
    length(x) == length(unique(x)) && length(x) == y && !any(is.na(x))
}

.internal.assign.lab.abort <- function(x = 'tip node'){
    cli::cli_abort(c(
       "The {.var label} must be a {.cls character} vector, and the length, ",
        paste0("of it must be equal to the number of ", x, ", "),
       "and NA or duplicated character are not be allowed"
    ), call = NULL)
}
