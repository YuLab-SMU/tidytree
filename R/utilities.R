
## ##' test whether input object is produced by ggtree function
## ##'
## ##'
## ##' @title is.ggtree
## ##' @param x object
## ##' @return TRUE or FALSE
## ##' @export
## ##' @author guangchuang yu
## is.ggtree <- function(x) inherits(x, 'ggtree')


## ##' @importFrom methods .hasSlot is missingArg new slot slot<-
## has.slot <- function(object, slotName) {
##     if (!isS4(object)) {
##         return(FALSE)
##     }
##     .hasSlot(object, slotName)
##     ## slot <- tryCatch(slot(object, slotName), error=function(e) NULL)
##     ## ! is.null(slot)
## }



is.tree <- function(x) inherits(x, "treedata")

## has.extraInfo <- function(object) {
##     if (!is.tree(object)) {
##         return(FALSE)
##     }

##     if (!has.slot(object, "extraInfo")) {
##         return(FALSE)
##     }

##     extraInfo <- object@extraInfo

##     if (nrow(extraInfo) > 0) {
##         return(TRUE)
##     }

##     return(FALSE)
## }

