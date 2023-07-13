##' @method mutate tbl_tree
##' @importFrom dplyr mutate
##' @export
mutate.tbl_tree <- function(.data, ...) {
    res <- NextMethod()
    if (!valid.tbl_tree2(res)){
        res <- drop_class(res, 'tbl_tree')
    }
    res
}

##' @method mutate treedata
##' @export
mutate.treedata <- function(.data, ..., keep.td=TRUE){
    dots <- rlang::quos(...)
    dat <- .extract_annotda.treedata(.data)
    da <- dplyr::mutate(dat, !!!dots)
    if (keep.td){
        if ('label' %in% names(dots)){
            .data@phylo$tip.label <- as.vector(da[da$isTip, 'label', drop = TRUE])
            if (!is.null(.data@phylo$node.label)){
                .data@phylo$node.label <- as.vector(da[!da$isTip, 'label', drop = TRUE])
            }
        }
        .data <- .update.treedata(td = .data, 
                                  da = da, 
                                  dat = dat, 
                                  type = "extra")
        return(.data)
    }
    return(da)
}
