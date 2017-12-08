##' @method sibling tree_tbl
##' @export
sibling.tree_tbl <- function(.data, .node, ...) {
    p <- parent(.data, .node)
    if (nrow(p) == 0) # if root node, return empty tibble
        return(p)
    child(.data, p$node) %>% filter_(~node != .node)
}
