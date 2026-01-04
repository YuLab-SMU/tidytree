#' @importFrom ape as.phylo
#' @export
ape::as.phylo


#' convert a tree object to treedata object
#'
#'
#' @title as.treedata
#' @param tree tree object
#' @param ... additional parameters
#' @return treedata object
#' @rdname as.treedata
#' @export
as.treedata <- function(tree, ...) {
    UseMethod("as.treedata")
}


#' @docType methods
#' @name get.fields
#' @rdname get.fields-methods
#' @title get.fields method
#' @param object `treedata` object
#' @param ... additional parameter
#' @return available annotation variables
#' @export
setGeneric("get.fields", function(object, ...) standardGeneric("get.fields"))

#' @docType methods
#' @name get.data
#' @rdname get.data-methods
#' @title get.data method
#' @param object `treedata` object
#' @param ... additional parameter
#' @return associated data of phylogeny
#' @export
setGeneric("get.data", function(object, ...) standardGeneric("get.data"))


#' grouping OTUs
#'
#'
#' @title groupOTU
#' @param .data tree object (phylo, treedata, tbl_tree, ggtree etc.)
#' @param .node selected nodes
#' @param group_name character the name of the group cluster, default is \code{group}.
#' @param ... additional parameter
#' @return updated tree with group information or group index
#' @author Guangchuang Yu
#' @export
groupOTU <- function(.data, .node, group_name = 'group', ...) {
    UseMethod("groupOTU")
}

#' grouping clades
#'
#'
#' @title groupClade
#' @inheritParams groupOTU
#' @param overlap character one of \code{overwrite},\code{origin} and \code{abandon},
#' default is \code{overwrite}.
#' @return updated tree with group information or group index
#' @author Guangchuang Yu
#' @export
groupClade <- function(.data, .node, group_name = 'group', overlap = 'overwrite', ...) {
    UseMethod("groupClade")
}

#' access child data
#'
#'
#' @title child
#' @rdname child
#' @param .data phylo or tbl_tree object
#' @param .node node number
#' @param ... additional parameters
#' @return child data
#' @export
#' @author Guangchuang Yu
child <- function(.data, .node, ...) {
    UseMethod("child")
}

#' access offspring data
#'
#'
#' @title offspring
#' @rdname offspring
#' @inheritParams child
#' @param tiponly whether only return tip nodes
#' @param self_include whether include the input node,
#' only applicable for tiponly = FALSE
#' @return offspring data
#' @export
#' @author Guangchuang Yu
offspring <- function(.data, .node, tiponly, self_include, ...) {
    UseMethod("offspring")
}


#' access parent data
#'
#'
#' @title parent
#' @rdname parent
#' @inheritParams child
#' @return parent data
#' @export
#' @author Guangchuang Yu
parent <- function(.data, .node, ...) {
    UseMethod("parent")
}


#' access ancestor data
#'
#'
#' @title ancestor
#' @rdname ancestor
#' @inheritParams child
#' @return ancestor data
#' @export
#' @author Guangchuang Yu
ancestor <- function(.data, .node, ...) {
    UseMethod("ancestor")
}

#' access most recent common ancestor data
#'
#'
#' @title MRCA
#' @rdname MRCA
#' @param .data phylo or tbl_tree object
#' @param ... additional parameters
#' @return MRCA data
#' @export
#' @author Guangchuang Yu
MRCA <- function(.data, ...) {
    UseMethod("MRCA")
}


#' access root node data
#'
#'
#' @title rootnode
#' @rdname rootnode
#' @inheritParams child
#' @return root node data
#' @export
#' @author Guangchuang Yu
rootnode <- function(.data, ...) {
    UseMethod("rootnode")
}

#' access sibling data
#'
#'
#' @title sibling
#' @rdname sibling
#' @inheritParams child
#' @return sibling
#' @export
#' @author Guangchuang Yu
sibling <- function(.data, ...) {
    UseMethod("sibling")
}

#' convert tree label to internal node number
#'
#'
#' @title nodeid
#' @rdname nodeid
#' @param tree tree object
#' @param label tip/node label(s)
#' @return node number
#' @export
#' @author Guangchuang Yu
nodeid <- function(tree, label) {
    UseMethod("nodeid")
}

#' convert internal node number tip/node label
#'
#'
#' @title nodelab
#' @rdname nodelab
#' @param tree tree object
#' @param id node number
#' @return tip/node label(s)
#' @export
#' @author Guangchuang Yu
nodelab <- function(tree, id) {
    UseMethod("nodelab")
}

#' @docType methods
#' @name drop.tip
#' @rdname drop.tip-methods
#' @title drop.tip method
#' @param object A treedata or phylo object
#' @param tip a vector of mode numeric or character specifying the tips to delete
#' @param ... additional parameters
#' @return updated object
#' @export
setGeneric (
    name = "drop.tip",
    def = function( object, tip, ... )
        standardGeneric("drop.tip")
)

#' @rdname drop.tip-methods
#' @export
setGeneric(
    name = 'keep.tip',
    def = function(object, tip, ...)
        standardGeneric('keep.tip')
)

#' whether the node is a tip
#'
#'
#' @title isTip
#' @param .data phylo, treedata or tbl_tree object
#' @param .node node number
#' @param ... additional parameters
#' @return logical value
#' @export
#' @author Guangchuang Yu
isTip <- function(.data, .node, ...) {
    UseMethod("isTip")
}

#' access tree text (newick text) from tree object
#'
#'
#' @docType methods
#' @name get.treetext
#' @rdname get.treetext-methods
#' @title get.treetext method
#' @param object treedata object
#' @param ... additional parameter
#' @return phylo object
#' @importFrom methods setGeneric
#' @export
setGeneric(
    name = "get.treetext",
    def = function(object, ...)
        standardGeneric("get.treetext")
)
