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

add_class <- function(x, name){
    xx <- setdiff(name, class(x))
    if (length(xx)>0){
        class(x) <- base::union(xx, class(x))
    }
    return (x)
}

nodeIds <- function(tree, internal.only=TRUE) {
    if (internal.only) {
        return(Ntip(tree) + 1:Nnode(tree, internal.only))
    }
    1:Nnode(tree, internal.only)
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

.rev.edge <- function(x, nodes, index){
    ind <- x[,index] %in% nodes
    x[ind,] <- t(apply(x[ind,],1,rev))
    return(x)
}

.check.no.tree.network <- function(x, nodes){
    is.tree <- length(table(x)) - nrow(x) != 1
    is.tree || any(((x[,1] %in% nodes) + (x[,2] %in% nodes)) ==2)
}

tbl_df_returned_message <- "# A tbl_df is returned for independent data analysis."

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

is_numeric <- function(x) !anyNA(suppressWarnings(as.numeric(as.character(x))))

filename <- function(file) {
    ## textConnection(text_string) will work just like a file
    ## in this case, just set the filename as ""
    file_name <- ""
    if (is.character(file)) {
        file_name <- file
    }
    return(file_name)
}
