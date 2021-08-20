
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
