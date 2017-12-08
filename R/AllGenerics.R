##' access child data
##'
##'
##' @title child
##' @param .data A tbl_tree data frame
##' @param ... additional parameters
##' @return tbl_tree data frame
##' @export
##' @author guangchuang yu
child <- function(.data, ...) {
    UseMethod("child")
}

##' access offspring data
##'
##'
##' @title offspring
##' @inheritParams child
##' @return tbl_tree data frame
##' @export
##' @author guangchuang yu
offspring <- function(.data, ...) {
    UseMethod("offspring")
}


##' access parent data
##'
##'
##' @title parent
##' @inheritParams child
##' @return tbl_tree data frame
##' @export
##' @author guangchuang yu
parent <- function(.data, ...) {
    UseMethod("parent")
}


##' access ancestor data
##'
##'
##' @title ancestor
##' @inheritParams child
##' @return tbl_tree data frame
##' @export
##' @author guangchuang yu
ancestor <- function(.data, ...) {
    UseMethod("ancestor")
}

##' access most recent common ancestor data
##'
##'
##' @title mrca
##' @inheritParams child
##' @return tbl_tree data frame
##' @export
##' @author guangchuang yu
mrca <- function(.data, ...) {
    UseMethod("mrca")
}


##' access root data
##'
##'
##' @title root
##' @inheritParams child
##' @return tbl_tree data frame
##' @export
##' @author guangchuang yu
root <- function(.data, ...) {
    UseMethod("root")
}

##' access sibling data
##'
##'
##' @title sibling
##' @inheritParams child
##' @return tbl_tree data frame
##' @export
##' @author guangchuang yu
sibling <- function(.data, ...) {
    UseMethod("sibling")
}
