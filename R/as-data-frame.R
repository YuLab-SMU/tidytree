##' @method as.data.frame treedata
##' @export
##' @importFrom treeio Nnode
##' @importFrom treeio Ntip
as.data.frame.treedata <- function(x, row.names, optional, branch.length = "branch.length", ...) {
    tree <- set_branch_length(x, branch.length)

    res <- as.data.frame(tree@phylo)
    tree_anno <- get_tree_data(x)
    if (nrow(tree_anno) > 0) {
        res <- merge(res, tree_anno, by="node", all.x=TRUE)
    }
    return(res)
}

##' @method as.data.frame phylo
##' @export
as.data.frame.phylo <- function(x, row.names, optional, branch.length = "branch.length", ...) {
    phylo <- x
    ntip <- Ntip(phylo)
    N <- Nnode(phylo, internal.only=FALSE)

    tip.label <- phylo[["tip.label"]]
    res <- as.data.frame(phylo[["edge"]])
    colnames(res) <- c("parent", "node")
    if (!is.null(phylo$edge.length))
        res$branch.length <- phylo$edge.length

    label <- rep(NA, N)
    label[1:ntip] <- tip.label
    if ( !is.null(phylo$node.label) ) {
        label[(ntip+1):N] <- phylo$node.label
    }
    label.df <- data.frame(node=1:N, label=label)
    res <- merge(res, label.df, by='node', all.y=TRUE)
    isTip <- rep(FALSE, N)
    isTip[1:ntip] <- TRUE
    res$isTip <- isTip

    return(res)
}

