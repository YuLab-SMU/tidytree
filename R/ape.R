
#' @importFrom ape Nnode
#' @export
ape::Nnode

#' @importFrom ape rtree
#' @export
ape::rtree

#' @importFrom ape read.tree
#' @export
ape::read.tree

#' @importFrom ape Ntip
#' @export
ape::Ntip

#' @importFrom ape as.phylo
#' @export
ape::as.phylo

#' @importFrom ape is.rooted
#' @export
ape::is.rooted

#' @importFrom ape root
#' @export
ape::root

#' @method is.rooted treedata
#' @importFrom ape is.rooted
#' @export
is.rooted.treedata <- function(phy) {
    is.rooted(as.phylo(phy))
}


#' @method Ntip treedata
#' @importFrom ape Ntip
#' @export
Ntip.treedata <- function(phy) {
    Ntip(as.phylo(phy))
}

#' @method Ntip treedataList
#' @importFrom ape Ntip
#' @export
Ntip.treedataList <- function(phy) {
    Ntip(phy[[1]])
}

#' number of nodes
#'
#'
#' @title Nnode
#' @param phy treedata object
#' @param internal.only whether only count internal nodes
#' @param ... additional parameters
#' @return number of nodes
#' @method Nnode treedata
#' @export
#' @examples
#' Nnode(rtree(30))
#' @author Guangchuang Yu
Nnode.treedata <- function(phy, internal.only=TRUE, ...) {
    Nnode(as.phylo(phy), internal.only = internal.only, ...)
}
