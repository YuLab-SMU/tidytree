##' @importFrom ggplot2 fortify
##' @method fortify phylo
##' @export
fortify.phylo <- function(model, data, layout="rectangular", branch.length ="branch.length",
                             ladderize=TRUE, right=FALSE, mrsd=NULL, ...) {
    model <- set_branch_length(model, branch.length)
    x <- reorder.phylo(get.tree(model), "postorder")
    if (is.null(x$edge.length) || branch.length == "none") {
        xpos <- getXcoord_no_length(x)
    } else {
        xpos <- getXcoord(x)
    }
    ypos <- getYcoord(x)
    N <- Nnode(x, internal.only=FALSE)
    xypos <- data.frame(node=1:N, x=xpos, y=ypos)

    df <- as.data.frame(model, branch.length="branch.length") # already set by set_branch_length
    idx <- is.na(df$parent)
    df$parent[idx] <- df$node[idx]
    rownames(df) <- df$node

    res <- merge(df, xypos, by='node', all.y=TRUE)

    ## add branch mid position
    res <- calculate_branch_mid(res)

    ## ## angle for all layout, if 'rectangular', user use coord_polar, can still use angle
    res <- calculate_angle(res)
    res
}

##' @method fortify treedata
##' @export
fortify.treedata <- fortify.phylo


calculate_angle <- function(data) {
    data$angle <- 360/(diff(range(data$y)) + 1) * data$y
    return(data)
}
