#' @method left_join treedata
#' @export
left_join.treedata <- function(x, y, by = NULL, copy = FALSE, ...){
    dots <- rlang::quos(...)
    suffix <- c("", ".y")
    if ("suffix" %in% names(dots)){
        dots <- dots[names(dots)!="suffix"]
    }

    dat <- .extract_annotda.treedata(x)
    ornm <- colnames(dat) 
    da <- dat %>%
          dplyr::left_join(y, by = by, copy = copy, suffix = suffix, !!!dots)

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


#' @keywords internal
#' @param td treedata object
#' @param da tbl_df after left_join.
#' @noRd
.update.td.join <- function(td, da){
    data.nm <- get.fields.data(td)
    if (length(data.nm)==1 && data.nm==""){
        td@data <- tibble()
    }else{
        td@data <- da %>% select(c("node", data.nm))
    }
    extra.nm <- colnames(da)[!colnames(da) %in% c("node", "label", "isTip", data.nm)]
    if (length(extra.nm) > 0){
        td@extraInfo <- da %>% select(c("node", extra.nm))
    }
    return(td)
}
