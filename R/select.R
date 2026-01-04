#' @importFrom dplyr select
#' @method select ggtree
#' @export
select.ggtree <- function(.data, ...) {
    dots <- rlang::quos(...)
    dplyr::select(.data$data, !!!dots)
}

#' @method select treedata
#' @export
select.treedata <- function(.data, ..., keep.td=FALSE){
    dots <- rlang::quos(...)
    dat <- .extract_annotda.treedata(.data)
    da <- dplyr::select(dat, !!!dots)
    if (keep.td){
        .data <- .update.treedata(td=.data, da=da, dat=dat)
        return(.data)
    }
    return(da)
}

#' @method select tbl_tree
#' @export
select.tbl_tree <- function(.data, ...){
    x <- NextMethod()
    if (!valid.tbl_tree2(x)){
        x <- drop_class(x, name = 'tbl_tree')
    }
    return(x)
}
