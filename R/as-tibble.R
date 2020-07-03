##' @method as_tibble phylo
##' @export
##' @importFrom dplyr full_join
##' @importFrom ape Ntip
##' @importFrom ape Nnode
as_tibble.phylo <- function(x, ...) {
    phylo <- x
    ntip <- Ntip(phylo)
    N <- Nnode(phylo, internal.only=FALSE)

    tip.label <- phylo[["tip.label"]]
    edge <- phylo[["edge"]]
    colnames(edge) <- c("parent", "node")
    res <- as_tibble(edge)
    if (!is.null(phylo$edge.length))
        res$branch.length <- phylo$edge.length

    label <- rep(NA, N)
    label[1:ntip] <- tip.label
    if ( !is.null(phylo$node.label) ) {
        label[(ntip+1):N] <- phylo$node.label
    }
    ## isTip <- rep(FALSE, N)
    ## isTip[1:ntip] <- TRUE

    label.df <- tibble(node=1:N, label=label) #, isTip = isTip)
    res <- full_join(res, label.df, by='node')

    idx <- is.na(res$parent)
    res$parent[idx] <- res$node[idx]

    if (!is.null(phylo$edge.length) && !is.null(phylo$root.edge))
        res$branch.length[res$parent == res$node] = phylo$root.edge

    res <- res[order(res$node),]
    aa <- names(attributes(phylo))
    group <- aa[ ! aa %in% c("names", "class", "order", "reroot", "node_map")]
    if (length(group) > 0) {
        for (group_ in group) {
            ## groupOTU & groupClade
            group_info <- attr(phylo, group_)
            if (length(group_info) == nrow(res)) {
                res[[group_]] <- group_info
            }
        }
    }
    class(res) <- c("tbl_tree", class(res))
    return(res)
}


##' @method as_tibble treedata
##' @importFrom tibble as_tibble
##' @export
as_tibble.treedata <- function(x, ...) {
    res <- as_tibble(x@phylo)
    tree_anno <- as_tibble(get_tree_data(x))
    if (nrow(tree_anno) > 0) {
        by <- "node"
        tree_anno$node <- as.integer(tree_anno$node)
        if ("parent" %in% colnames(tree_anno)) {
            by <- c(by, "parent")
            tree_anno$parent <- as.integer(tree_anno$parent)
        }
        res <- full_join(res, tree_anno, by=by)
    }
    return(res)
}


##' get associated data stored in treedata object
##'
##'
##' @title get_tree_data
##' @param tree_object a `treedata` object
##' @return tbl_df
##' @export
##' @author guangchuang yu
get_tree_data <- function(tree_object) {
    tree_anno <- tree_object@data
    extraInfo <- tree_object@extraInfo

    if (nrow(tree_anno) == 0) {
        extraInfo$node <- as.integer(extraInfo$node)
        return(extraInfo)
    }
    if (nrow(extraInfo) == 0) {
        tree_anno$node <- as.integer(tree_anno$node)
        return(tree_anno)
    }

    tree_anno$node <- as.integer(tree_anno$node)
    extraInfo$node <- as.integer(extraInfo$node)

    full_join(tree_anno, extraInfo, by = "node")
}


valid.tbl_tree <- function(object, cols = c("parent", "node", "label")) {
    cc <- cols[!cols %in% colnames(object)]
    if (length(cc) > 0) {
        msg <- paste0("invalid tbl_tree object.\n  missing column:\n    ", paste(cc, collapse=","), ".")
    }
}
