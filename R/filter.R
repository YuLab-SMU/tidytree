
##' @importFrom dplyr filter
##' @method filter ggtree
##' @export
filter.ggtree <- function(.data, ..., .preserve = FALSE) {
    dots <- rlang::quos(...)
    dplyr::filter(.data$data, !!!dots, .preserve = .preserve)
}

