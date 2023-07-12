.internal_add_isTip <- function(x){
    x %<>% mutate(isTip=ifelse(!.data$node %in% .data$parent, TRUE, FALSE))
    return(x)
}

.extract_annotda.treedata <- function(x){
    if (inherits(x, "treedata")){
       annotda <- get_tree_data(x)
       x <- x@phylo
    }else{
       annotda <- NULL
    }
    trdf <- x %>% 
            as_tibble() %>% 
            .internal_add_isTip() %>%
            drop_class(name="tbl_tree")

    if (!any(is.null(annotda) || nrow(annotda)==0)){
        annotda <- trdf %>% 
                   dplyr::left_join(annotda, by="node")
    }else{
        annotda <- trdf
    }
    annotda <- annotda[, !colnames(annotda) %in% c("parent", "branch.length")]
    return(annotda)
}

.update.treedata <- function(td, da, dat, type=NULL){
    if (inherits(td, "phylo")){
        td <- treedata(phylo=td)
    }
    data.nm <- get.fields.data(td)
    extra.nm <- get.fields.extraInfo(td) 
    data.nm <- intersect(data.nm, colnames(da))
    if (!is.null(type) && type == "extra"){
        clnm <- colnames(da)[!colnames(da) %in% c("label", "isTip", data.nm)]
        extra.nm <- union(extra.nm, clnm)
        dat <- da
    }else{
        extra.nm <- intersect(extra.nm, colnames(da))
    }
    if (length(data.nm)>0){
        td@data <- dat %>% select(c("node", data.nm))
    }else{
        td@data <- tibble()
    }
    if (length(extra.nm)>0){
        td@extraInfo <- dat %>% select(c("node", extra.nm))
    }else{
        td@extraInfo <- tibble()
    }
    return(td)
}

#' remove the some class names from x object
#' @noRd
drop_class <- function(x, name) {
    class(x) <- class(x)[!class(x) %in% name]
    x
}

.internal_nest <- function(x, keepnm, ..., .names_sep = NULL){
    nest <- utils::getFromNamespace("nest", "tidyr")
    if (missing(...)){
        idx <- x %>% vapply(is.list, logical(1))
        clnm <- colnames(x)
        clnm <- clnm[!idx]
        clnm <- clnm[!clnm %in% keepnm]
        params <- c(list(x), lapply(clnm, function(x)x))
        names(params) <- c(".data", clnm)
    }else{
        res <- nest(.data=x, ..., .names_sep=.names_sep)
        return(res)
    }
    if (!is.null(.names_sep)){
        params <- c(params, .names_sep=.names_sep)
    }
    res <- do.call(nest, params)
    return(res)
}

tbl_df_returned_message <- "# A tbl_df is returned for independent data analysis."

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
