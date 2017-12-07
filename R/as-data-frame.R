##' @method as_data_frame phylo
##' @export
##' @importFrom tibble data_frame
##' @importFrom dplyr full_join
as_data_frame.phylo <- function(x, ...) {
    phylo <- x
    ntip <- Ntip(phylo)
    N <- Nnode(phylo, internal.only=FALSE)

    tip.label <- phylo[["tip.label"]]
    res <- as_data_frame(phylo[["edge"]])
    colnames(res) <- c("parent", "node")
    if (!is.null(phylo$edge.length))
        res$branch.length <- phylo$edge.length

    label <- rep(NA, N)
    label[1:ntip] <- tip.label
    if ( !is.null(phylo$node.label) ) {
        label[(ntip+1):N] <- phylo$node.label
    }
    isTip <- rep(FALSE, N)
    isTip[1:ntip] <- TRUE

    label.df <- data_frame(node=1:N, label=label, isTip = isTip)
    res <- full_join(res, label.df, by='node')

    idx <- is.na(res$parent)
    res$parent[idx] <- res$node[idx]

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
    return(res)
}


##' @method as_data_frame treedata
##' @importFrom tibble as_data_frame
##' @export
##' @importFrom treeio Nnode
##' @importFrom treeio Ntip
##' @importFrom treeio get_tree_data
as_data_frame.treedata <- function(x, ...) {
    res <- as_data_frame(x@phylo)
    tree_anno <- as_data_frame(get_tree_data(x))
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


