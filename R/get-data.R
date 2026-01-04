#' get.data method
#'
#'
#' @rdname get.data-methods
#' @exportMethod get.data
setMethod("get.data", signature(object = "treedata"),
          function(object) {
              get_tree_data(object)
          })

#' @method [ treedata
#' @export
`[.treedata` <- function(x, i, j) {
    get.data(x)[i, j]
}


#' @method [[ treedata
#' @export
`[[.treedata` <- function(x, i) {
    get.data(x)[[i]]
}

