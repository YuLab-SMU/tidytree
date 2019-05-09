##' @importFrom methods setOldClass
setOldClass("phylo")
## setOldClass("multiPhylo")
setOldClass("DNAbin")

##' Class "treedata"
##' This class stores phylogenetic tree with associated data
##'
##'
##' @name treedata-class
##' @aliases treedata-class
##'   show,treedata-method
##' @docType class
##' @slot file tree file
##' @slot treetext newick tree string
##' @slot phylo phylo object for tree structure
##' @slot data associated data
##' @slot extraInfo extra information, reserve for merge_tree
##' @slot tip_seq tip sequences
##' @slot anc_seq ancestral sequences
##' @slot seq_type sequence type, one of NT or AA
##' @slot tipseq_file tip sequence file
##' @slot ancseq_file ancestral sequence file
##' @slot info extra information, e.g. metadata, software version etc.
##' @importFrom methods setClass
##' @importFrom methods representation
##' @importFrom ape as.DNAbin
##' @exportClass treedata
##' @author Guangchuang Yu <https://guangchuangyu.github.io>
##' @keywords classes
setClass("treedata",
         representation = representation(
             file        = "character",
             treetext    = "character",
             phylo       = "phylo",
             data        = "tbl_df",
             extraInfo   = "tbl_df",
             tip_seq     = "DNAbin",
             anc_seq     = "DNAbin",
             seq_type    = "character",
             tipseq_file = "character",
             ancseq_file = "character",
             info        = "list"
         ),
         prototype = prototype(
             data      = data_frame(),
             extraInfo = data_frame(),
             anc_seq = ape::as.DNAbin(character(0)),
             tip_seq = ape::as.DNAbin(character(0))
         )
         )

##' treedata object contructor
##'
##'
##' @title treedata
##' @param ... parameters
##' @return treedata object
##' @importFrom methods new
##' @export
##' @author guangchuang yu
treedata <- function(...) {
    new("treedata", ...)
}
