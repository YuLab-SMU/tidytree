##' @importFrom dplyr select
##' @method select ggtree
##' @export
select.ggtree <- function(.data, ...) {
    dots <- rlang::quos(...)
    dplyr::select(.data$data, !!!dots)
}

