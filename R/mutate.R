##' @method mutate tbl_tree
##' @importFrom dplyr mutate
##' @export
mutate.tbl_tree <- function(.data, ...) {
    res <- NextMethod()
    class(res) <- c("tbl_tree", class(res))
    res
}
