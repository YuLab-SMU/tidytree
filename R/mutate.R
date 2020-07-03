##' @method mutate tbl_tree
##' @importFrom dplyr mutate
##' @export
mutate.tbl_tree <- function(.data, ...) {
    res <- NextMethod()
    class(res) <- class(.data)
    res
}
