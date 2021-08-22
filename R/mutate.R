##' @method mutate tbl_tree
##' @importFrom dplyr mutate
##' @export
mutate.tbl_tree <- function(.data, ...) {
    res <- NextMethod()
    class(res) <- class(.data)
    res
}

##' @method mutate treedata
##' @export
mutate.treedata <- function(.data, ..., keep.td=TRUE){
    dots <- rlang::quos(...)
    dat <- .extract_annotda.treedata(.data)
    da <- dplyr::mutate(dat, !!!dots)
    if (keep.td){
        .data <- .update.treedata(td = .data, 
                                  da = da, 
                                  dat = dat, 
                                  type = "extra")
        return(.data)
    }
    return(da)
}
