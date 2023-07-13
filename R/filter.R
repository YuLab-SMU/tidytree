
##' @importFrom dplyr filter
##' @method filter ggtree
##' @export
filter.ggtree <- function(.data, ..., .preserve = FALSE) {
    dots <- rlang::quos(...)
    dplyr::filter(.data$data, !!!dots, .preserve = .preserve)
}

##' @method filter treedata
##' @export
filter.treedata <- function(.data, ..., .preserve=FALSE, keep.td=TRUE){
    dots <- rlang::quos(...)
    dat <- .extract_annotda.treedata(.data)
    da <- dplyr::filter(dat, !!!dots, .preserve = .preserve)
    if (keep.td){
        .data <- .update.treedata(td=.data, da=da, dat=dat, type="extra")
        return(.data)
    }
    return(da)
}


##' @method filter tbl_tree
##' @export
filter.tbl_tree <- function(.data, ..., .preserve = FALSE){
    x <- NextMethod()
    if (!valid.tbl_tree2(x)){
        x <- drop_class(x, name='tbl_tree')
    }
    return(x)
}
