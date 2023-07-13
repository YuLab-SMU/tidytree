#' @method left_join treedata
#' @importFrom cli cli_warn
#' @export
left_join.treedata <- function(x, y, by = NULL, copy = FALSE, suffix=c("", ".y"), ...){
    dat <- .extract_annotda.treedata(x)
    ornm <- colnames(dat)
    msg <- c("The {.arg suffix} requires a character vector containing 2 different elements,",
             "The first element must be \"\", and the second element must not be \"\",",
             "it was set {.code suffix=c(\"\", \".y\")} automatically.")
    if (all(nchar(suffix)!=0)){
        cli::cli_warn(msg)
        suffix[1] = ""
    }
    if (all(nchar(suffix)==0)){
        cli::cli_warn(msg)
        suffix[2] = ".y"
    }
    if (nchar(suffix[1])!=0 && nchar(suffix[2])==0){
        cli::cli_warn(msg)
        suffix <- rev(suffix[seq_len(2)])
    }
    da <- dplyr::left_join(dat, y, by = by, copy = copy, suffix = suffix, ...)

    if (any(duplicated(da$node))){
        da %<>% .internal_nest(keepnm=ornm)
    }

    tr <- .update.td.join(td=x, da=da)
    return(tr)
}

#' @method left_join phylo
#' @export
left_join.phylo <- function(x, y, by=NULL, copy=FALSE, ...){
    x <- treedata(phylo=x)
    tr <- x %>% left_join(y, by = by, copy = copy, ...)
    return(tr)
}

#' @method left_join tbl_tree
#' @export
left_join.tbl_tree <- function(x, y, by = NULL, copy = FALSE, 
                               suffix = c(".x", ".y"), ..., keep = NULL){
    x <- NextMethod()
    if (!valid.tbl_tree2(x)){
        x <- drop_class(x, 'tbl_tree')
    }
    return(x)
}


#' @keywords internal
#' @param td treedata object
#' @param da tbl_df after left_join.
#' @noRd
.update.td.join <- function(td, da){
    aa <- names(attributes(td@phylo))
    aa <- aa[!aa %in% c("names", "class", "order", "reroot", "node_map")]
    data.nm <- get.fields.data(td)
    if (length(data.nm)==1 && data.nm==""){
        td@data <- tibble()
    }else{
        td@data <- da %>% select(c("node", data.nm))
    }
    extra.nm <- colnames(da)[!colnames(da) %in% c("node", "label", "isTip", data.nm, aa)]
    if (length(extra.nm) > 0){
        td@extraInfo <- da %>% select(c("node", extra.nm))
    }
    return(td)
}
