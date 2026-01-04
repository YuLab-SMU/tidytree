#' @method pull treedata
#' @export
pull.treedata <- function(.data, var = -1, name = NULL, ...){
    var <- rlang::enquo(var)
    name <- rlang::enquo(name)
    dat <- .extract_annotda.treedata(.data)
    dplyr::pull(dat, var = !!var, name = !!name, ...)
}

#' @method pull phylo
#' @export
pull.phylo <- pull.treedata
