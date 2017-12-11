##' @importFrom treeio sibling
##' @method sibling tbl_tree
##' @export
sibling.tbl_tree <- function(.data, .node, ...) {
    p <- parent(.data, .node)
    if (nrow(p) == 0) # if root node, return empty tibble
        return(p)
    child(.data, p$node) %>% filter_(~node != .node)
}
