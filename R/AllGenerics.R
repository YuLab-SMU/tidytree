##' access child data
##'
##'
##' @title child
##' @param .data A tree_tbl data frame
##' @param ... additional parameters
##' @return tree_tbl data frame
##' @export
##' @author guangchuang yu
child <- function(.data, ...) {
    UseMethod("child")
}

##' access offspring data
##'
##'
##' @title offspring
##' @param .data A tree_tbl data frame
##' @param ... additional parameter
##' @return tree_tbl data frame
##' @export
##' @author guangchuang yu
offspring <- function(.data, ...) {
    UseMethod("offspring")
}


##' access parent data
##'
##'
##' @title parent
##' @param .data A tree_tbl data frame
##' @param ... additional parameter
##' @return tree_tbl data frame
##' @export
##' @author guangchuang yu
parent <- function(.data, ...) {
    UseMethod("parent")
}


##' access ancestor data
##'
##'
##' @title ancestor
##' @param .data A tree_tbl data frame
##' @param ... additional parameter
##' @return tree_tbl data frame
##' @export
##' @author guangchuang yu
ancestor <- function(.data, ...) {
    UseMethod("ancestor")
}


##' access sibling data
##'
##'
##' @title sibling
##' @param .data A tree_tbl data frame
##' @param ... additional parameter
##' @return tree_tbl data frame
##' @export
##' @author guangchuang yu
sibling <- function(.data, ...) {
    UseMethod("sibling")
}
