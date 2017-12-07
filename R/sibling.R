##' @method sibling tree_tbl
##' @export
sibling.tree_tbl <- function(.data, .node, ...) {
    p <- parent(.data, .node)
    child(.data, p$node) %>% filter_(~node != .node)
}
