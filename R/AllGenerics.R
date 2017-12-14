##' @importFrom ape as.phylo
##' @export
ape::as.phylo


##' convert a tree object to treedata object
##'
##'
##' @title as.treedata
##' @param tree tree object
##' @param ... additional parameters
##' @return treedata object
##' @rdname as.treedata
##' @export
as.treedata <- function(tree, ...) {
    UseMethod("as.treedata")
}


##' @docType methods
##' @name get.fields
##' @rdname get.fields-methods
##' @title get.fields method
##' @param object \code{treedata} object
##' @param ... additional parameter
##' @return available annotation variables
##' @export
setGeneric("get.fields", function(object, ...) standardGeneric("get.fields"))


##' grouping OTUs
##'
##'
##' @title groupOTU
##' @param .data tree object (phylo, treedata, tbl_tree, ggtree etc.)
##' @param .node selected nodes
##' @param ... additional parameter
##' @return updated tree with group information or group index
##' @author guangchuang yu
##' @export
groupOTU <- function(.data, .node, ...) {
    UseMethod("groupOTU")
}

##' grouping clades
##'
##'
##' @title groupClade
##' @inheritParams groupOTU
##' @return updated tree with group information or group index
##' @author Guangchuang Yu
##' @export
groupClade <- function(.data, .node, ...) {
    UseMethod("groupClade")
}

##' access child data
##'
##'
##' @title child
##' @rdname child
##' @param .data phylo or tbl_tree object
##' @param .node node number
##' @param ... additional parameters
##' @return child data
##' @export
##' @author guangchuang yu
child <- function(.data, .node, ...) {
    UseMethod("child")
}

##' access offspring data
##'
##'
##' @title offspring
##' @rdname offspring
##' @inheritParams child
##' @return offspring data
##' @export
##' @author guangchuang yu
offspring <- function(.data, .node, ...) {
    UseMethod("offspring")
}


##' access parent data
##'
##'
##' @title parent
##' @rdname parent
##' @inheritParams child
##' @return parent data
##' @export
##' @author guangchuang yu
parent <- function(.data, .node, ...) {
    UseMethod("parent")
}


##' access ancestor data
##'
##'
##' @title ancestor
##' @rdname ancestor
##' @inheritParams child
##' @return ancestor data
##' @export
##' @author guangchuang yu
ancestor <- function(.data, .node, ...) {
    UseMethod("ancestor")
}

##' access most recent common ancestor data
##'
##'
##' @title MRCA
##' @rdname MRCA
##' @param .data phylo or tbl_tree object
##' @param ... additional parameters
##' @return MRCA data
##' @export
##' @author guangchuang yu
MRCA <- function(.data, ...) {
    UseMethod("MRCA")
}


##' access root node data
##'
##'
##' @title rootnode
##' @rdname rootnode
##' @inheritParams child
##' @return root node data
##' @export
##' @author guangchuang yu
rootnode <- function(.data, ...) {
    UseMethod("rootnode")
}

##' access sibling data
##'
##'
##' @title sibling
##' @rdname sibling
##' @inheritParams child
##' @return sibling
##' @export
##' @author guangchuang yu
sibling <- function(.data, ...) {
    UseMethod("sibling")
}

