##' @method merge tbl_tree
##' @export
merge.tbl_tree <- function(x, y, ...) {
    res <- NextMethod()
    class(res) <- class(x)
    return(res)
}

