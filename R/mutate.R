##' @method mutate tbl_tree
##' @importFrom dplyr mutate
##' @importFrom rlang quos
##' @importFrom utils getFromNamespace
##' @export
mutate.tbl_tree <- function(.data, ...) {
    dots  <- quos(...)
    mutate.tbl_df <- utils::getFromNamespace("mutate.tbl_df", "dplyr")
    res <- mutate.tbl_df(.data, !!! dots)
    class(res) <- c("tbl_tree", class(res))
    res
}

