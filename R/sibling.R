##' @method sibling tbl_tree
##' @export
sibling.tbl_tree <- function(.data, .node, ...) {
    valid.tbl_tree(.data)

    p <- parent(.data, .node)
    if (nrow(p) == 0) # if root node, return empty tibble
        return(p)
    child(.data, p$node) %>% filter(.data$node != .node)
}
