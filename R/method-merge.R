#' @method merge tbl_tree
#' @export
merge.tbl_tree <- function(x, y, ...) {
    res <- NextMethod()
    if (valid.tbl_tree2(res)){
        res <- add_class(res, 'tbl_tree')
    }
    return(res)
}

