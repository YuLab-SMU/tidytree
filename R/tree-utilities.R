##' convert tip or node label(s) to internal node number
##'
##'
##' @title nodeid
##' @param x tree object or graphic object return by ggtree
##' @param label tip or node label(s)
##' @return internal node number
##' @importFrom methods is
##' @export
##' @author Guangchuang Yu
nodeid <- function(x, label) {
    if (is(x, "gg"))
        return(nodeid.gg(x, label))

    nodeid.tree(x, label)
}

nodeid.tree <- function(tree, label) {
    tr <- get.tree(tree)
    lab <- c(tr$tip.label, tr$node.label)
    match(label, lab)
}

nodeid.gg <- function(p, label) {
    p$data$node[match(label, p$data$label)]
}


reroot_node_mapping <- function(tree, tree2) {
    root <- getRoot(tree)

    node_map <- data.frame(from=1:getNodeNum(tree), to=NA, visited=FALSE)
    node_map[1:Ntip(tree), 2] <- match(tree$tip.label, tree2$tip.label)
    node_map[1:Ntip(tree), 3] <- TRUE

    node_map[root, 2] <- root
    node_map[root, 3] <- TRUE

    node <- rev(tree$edge[,2])
    for (k in node) {
        ip <- getParent(tree, k)
        if (node_map[ip, "visited"])
            next

        cc <- getChild(tree, ip)
        node2 <- node_map[cc,2]
        if (anyNA(node2)) {
            node <- c(node, k)
            next
        }

        to <- unique(sapply(node2, getParent, tr=tree2))
        to <- to[! to %in% node_map[,2]]
        node_map[ip, 2] <- to
        node_map[ip, 3] <- TRUE
    }
    node_map <- node_map[, -3]
    return(node_map)
}



##' Get parent node id of child node.
##'
##' @title getParent.df
##' @param df tree data.frame
##' @param node is the node id of child in tree.
##' @return integer node id of parent
getParent.df <- function(df, node) {
    i <- which(df$node == node)
    parent_id <- df$parent[i]
    if (parent_id == node | is.na(parent_id)) {
        ## root node
        return(0)
    }
    return(parent_id)
}


getAncestor.df <- function(df, node) {
    anc <- getParent.df(df, node)
    anc <- anc[anc != 0]
    if (length(anc) == 0) {
        # stop("selected node is root...")
      return(0)
    }
    i <- 1
    while(i<= length(anc)) {
        anc <- c(anc, getParent.df(df, anc[i]))
        anc <- anc[anc != 0]
        i <- i+1
    }
    return(anc)
}



##' Get list of child node id numbers of parent node
##'
##' @title getChild.df
##' @param df tree data.frame
##' @param node is the node id of child in tree.
##' @return list of child node ids of parent
getChild.df <- function(df, node) {
    i <- which(df$parent == node)
    if (length(i) == 0) {
        return(0) # it has no children, hence tip node.
    }
    res <- df$node[i]
    res <- res[res != node] ## node may root
    return(res)
}

get.offspring.df <- function(df, node) {
    sp <- getChild.df(df, node)
    sp <- sp[sp != 0] # Remove root node.
    if (length(sp) == 0) {
        #stop("input node is a tip...")
      return(0)
    }

    i <- 1
    while(i <= length(sp)) {
        sp <- c(sp, getChild.df(df, sp[i]))
        sp <- sp[sp != 0]
        i <- i + 1
    }
    return(sp)
}



##' extract offspring tips
##'
##'
##' @title get.offspring.tip
##' @param tr tree
##' @param node node
##' @return tip label
##' @author ygc
##' @importFrom ape extract.clade
##' @export
get.offspring.tip <- function(tr, node) {
    if ( ! node %in% tr$edge[,1]) {
        ## return itself
        return(tr$tip.label[node])
    }
    clade <- extract.clade(tr, node)
    clade$tip.label
}




getParent <- function(tr, node) {
    if ( node == getRoot(tr) )
        return(0)
    edge <- tr[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]
    res <- parent[child == node]
    if (length(res) == 0) {
        stop("cannot found parent node...")
    }
    if (length(res) > 1) {
        stop("multiple parent found...")
    }
    return(res)
}




getChild <- function(tr, node) {
    # Get edge matrix from phylo object.
    edge <- tr[["edge"]]
    # Select all rows that match "node".
    res <- edge[edge[,1] == node, 2]
    ## if (length(res) == 0) {
    ##     ## is a tip
    ##     return(NA)
    ## }
    return(res)
}


getSibling <- function(tr, node) {
    root <- getRoot(tr)
    if (node == root) {
        return(NA)
    }

    parent <- getParent(tr, node)
    child <- getChild(tr, parent)
    sib <- child[child != node]
    return(sib)
}


getAncestor <- function(tr, node) {
    root <- getRoot(tr)
    if (node == root) {
        return(NA)
    }
    parent <- getParent(tr, node)
    res <- parent
    while(parent != root) {
        parent <- getParent(tr, parent)
        res <- c(res, parent)
    }
    return(res)
}


isRoot <- function(tr, node) {
    getRoot(tr) == node
}

getNodeName <- function(tr) {
    if (is.null(tr$node.label)) {
        n <- length(tr$tip.label)
        nl <- (n + 1):(2 * n - 2)
        nl <- as.character(nl)
    }
    else {
        nl <- tr$node.label
    }
    nodeName <- c(tr$tip.label, nl)
    return(nodeName)
}



get.trunk <- function(tr) {
    root <- getRoot(tr)
    path_length <- sapply(1:(root-1), function(x) get.path_length(tr, root, x))
    i <- which.max(path_length)
    return(get.path(tr, root, i))
}

##' path from start node to end node
##'
##'
##' @title get.path
##' @param phylo phylo object
##' @param from start node
##' @param to end node
##' @return node vectot
##' @export
##' @author Guangchuang Yu
get.path <- function(phylo, from, to) {
    anc_from <- getAncestor(phylo, from)
    anc_from <- c(from, anc_from)
    anc_to <- getAncestor(phylo, to)
    anc_to <- c(to, anc_to)
    mrca <- intersect(anc_from, anc_to)[1]

    i <- which(anc_from == mrca)
    j <- which(anc_to == mrca)

    path <- c(anc_from[1:i], rev(anc_to[1:(j-1)]))
    return(path)
}


get.path_length <- function(phylo, from, to, weight=NULL) {
    path <- get.path(phylo, from, to)
    if (is.null(weight)) {
        return(length(path)-1)
    }

    df <- fortify(phylo)
    if ( ! (weight %in% colnames(df))) {
        stop("weight should be one of numerical attributes of the tree...")
    }

    res <- 0

    get_edge_index <- function(df, from, to) {
        which((df[,1] == from | df[,2] == from) &
                  (df[,1] == to | df[,2] == to))
    }

    for(i in 1:(length(path)-1)) {
        ee <- get_edge_index(df, path[i], path[i+1])
        res <- res + df[ee, weight]
    }

    return(res)
}

##' @importFrom ape reorder.phylo
getNodes_by_postorder <- function(tree) {
  tree <- reorder.phylo(tree, "postorder")
    unique(rev(as.vector(t(tree$edge[,c(2,1)]))))
}

getXcoord2 <- function(x, root, parent, child, len, start=0, rev=FALSE) {
    x[root] <- start
    x[-root] <- NA  ## only root is set to start, by default 0

    currentNode <- root
    direction <- 1
    if (rev == TRUE) {
        direction <- -1
    }

    while(anyNA(x)) {
        idx <- which(parent %in% currentNode)
        newNode <- child[idx]
        x[newNode] <- x[parent[idx]]+len[idx] * direction
        currentNode <- newNode
    }

    return(x)
}






getXcoord_no_length <- function(tr) {
    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]
    root <- getRoot(tr)

    len <- tr$edge.length

    N <- getNodeNum(tr)
    x <- numeric(N)
    ntip <- Ntip(tr)
    currentNode <- 1:ntip
    x[-currentNode] <- NA

    cl <- split(child, parent)
    child_list <- list()
    child_list[as.numeric(names(cl))] <- cl

    while(anyNA(x)) {
        idx <- match(currentNode, child)
        pNode <- parent[idx]
        ## child number table
        p1 <- table(parent[parent %in% pNode])
        p2 <- table(pNode)
        np <- names(p2)
        i <- p1[np] == p2
        newNode <- as.numeric(np[i])

        exclude <- rep(NA, max(child))
        for (j in newNode) {
            x[j] <- min(x[child_list[[j]]]) - 1
            exclude[child_list[[j]]] <- child_list[[j]]
        }
        exclude <- exclude[!is.na(exclude)]

        ## currentNode %<>% `[`(!(. %in% exclude))
        ## currentNode %<>% c(., newNode) %>% unique
        currentNode <- currentNode[!currentNode %in% exclude]
        currentNode <- unique(c(currentNode, newNode))

    }
    x <- x - min(x)
    return(x)
}




getXcoord <- function(tr) {
    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]
    root <- getRoot(tr)

    len <- tr$edge.length

    N <- getNodeNum(tr)
    x <- numeric(N)
    x <- getXcoord2(x, root, parent, child, len)
    return(x)
}




## scale the branch (the line plotted) to the actual value of edge length
## but it seems not the good idea as if we want to add x-axis (e.g. time-scaled tree)
## then the x-value is not corresponding to edge length as in rectangular layout
## getXYcoord_slanted <- function(tr) {
##     edge <- tr$edge
##     parent <- edge[,1]
##     child <- edge[,2]
##     root <- getRoot(tr)

##     N <- getNodeNum(tr)
##     len <- tr$edge.length
##     y <- getYcoord(tr, step=min(len)/2)
##     len <- sqrt(len^2 - (y[parent]-y[child])^2)
##     x <- numeric(N)
##     x <- getXcoord2(x, root, parent, child, len)
##     res <- data.frame(x=x, y=y)
##     return(res)
## }



## @importFrom magrittr %>%
##' @importFrom magrittr equals
getYcoord <- function(tr, step=1) {
    Ntip <- length(tr[["tip.label"]])
    N <- getNodeNum(tr)

    edge <- tr[["edge"]]
    parent <- edge[,1]
    child <- edge[,2]

    cl <- split(child, parent)
    child_list <- list()
    child_list[as.numeric(names(cl))] <- cl

    y <- numeric(N)
    tip.idx <- child[child <= Ntip]
    y[tip.idx] <- 1:Ntip * step
    y[-tip.idx] <- NA

    ## use lookup table
    pvec <- integer(max(tr$edge))
    pvec[child] = parent

    currentNode <- 1:Ntip
    while(anyNA(y)) {
        ## pNode <- unique(parent[child %in% currentNode])
        pNode <- unique(pvec[currentNode])

        ## piping of magrittr is slower than nested function call.
        ## pipeR is fastest, may consider to use pipeR
        ##
        ## child %in% currentNode %>% which %>% parent[.] %>% unique
        ## idx <- sapply(pNode, function(i) all(child[parent == i] %in% currentNode))
        idx <- sapply(pNode, function(i) all(child_list[[i]] %in% currentNode))
        newNode <- pNode[idx]

        y[newNode] <- sapply(newNode, function(i) {
            mean(y[child_list[[i]]], na.rm=TRUE)
            ##child[parent == i] %>% y[.] %>% mean(na.rm=TRUE)
        })

        currentNode <- c(currentNode[!currentNode %in% unlist(child_list[newNode])], newNode)
        ## currentNode <- c(currentNode[!currentNode %in% child[parent %in% newNode]], newNode)
        ## parent %in% newNode %>% child[.] %>%
        ##     `%in%`(currentNode, .) %>% `!` %>%
        ##         currentNode[.] %>% c(., newNode)
    }

    return(y)
}


getYcoord_scale <- function(tr, df, yscale) {

    N <- getNodeNum(tr)
    y <- numeric(N)

    root <- getRoot(tr)
    y[root] <- 0
    y[-root] <- NA

    edge <- tr$edge
    parent <- edge[,1]
    child <- edge[,2]

    currentNodes <- root
    while(anyNA(y)) {
        newNodes <- c()
        for (currentNode in currentNodes) {
            idx <- which(parent %in% currentNode)
            newNode <- child[idx]
            direction <- -1
            for (i in seq_along(newNode)) {
                y[newNode[i]] <- y[currentNode] + df[newNode[i], yscale] * direction
                direction <- -1 * direction
            }
            newNodes <- c(newNodes, newNode)
        }
        currentNodes <- unique(newNodes)
    }
    if (min(y) < 0) {
        y <- y + abs(min(y))
    }
    return(y)
}


getYcoord_scale2 <- function(tr, df, yscale) {
    root <- getRoot(tr)

    pathLength <- sapply(1:length(tr$tip.label), function(i) {
        get.path_length(tr, i, root, yscale)
    })

    ordered_tip <- order(pathLength, decreasing = TRUE)
    ii <- 1
    ntip <- length(ordered_tip)
    while(ii < ntip) {
        sib <- getSibling(tr, ordered_tip[ii])
        if (length(sib) == 0) {
            ii <- ii + 1
            next
        }
        jj <- which(ordered_tip %in% sib)
        if (length(jj) == 0) {
            ii <- ii + 1
            next
        }
        sib <- ordered_tip[jj]
        ordered_tip <- ordered_tip[-jj]
        nn <- length(sib)
        if (ii < length(ordered_tip)) {
            ordered_tip <- c(ordered_tip[1:ii],sib, ordered_tip[(ii+1):length(ordered_tip)])
        } else {
            ordered_tip <- c(ordered_tip[1:ii],sib)
        }

        ii <- ii + nn + 1
    }


    long_branch <- getAncestor(tr, ordered_tip[1]) %>% rev
    long_branch <- c(long_branch, ordered_tip[1])

    N <- getNodeNum(tr)
    y <- numeric(N)

    y[root] <- 0
    y[-root] <- NA

    ## yy <- df[, yscale]
    ## yy[is.na(yy)] <- 0

    for (i in 2:length(long_branch)) {
        y[long_branch[i]] <- y[long_branch[i-1]] + df[long_branch[i], yscale]
    }

    parent <- df[, "parent"]
    child <- df[, "node"]

    currentNodes <- root
    while(anyNA(y)) {
        newNodes <- c()
        for (currentNode in currentNodes) {
            idx <- which(parent %in% currentNode)
            newNode <- child[idx]
            newNode <- c(newNode[! newNode %in% ordered_tip],
                         rev(ordered_tip[ordered_tip %in% newNode]))
            direction <- -1
            for (i in seq_along(newNode)) {
                if (is.na(y[newNode[i]])) {
                    y[newNode[i]] <- y[currentNode] + df[newNode[i], yscale] * direction
                    direction <- -1 * direction
                }
            }
            newNodes <- c(newNodes, newNode)
        }
        currentNodes <- unique(newNodes)
    }
    if (min(y) < 0) {
        y <- y + abs(min(y))
    }
    return(y)
}



getYcoord_scale_numeric <- function(tr, df, yscale, ...) {
    df <- .assign_parent_status(tr, df, yscale)
    df <- .assign_child_status(tr, df, yscale)

    y <- df[, yscale]

    if (anyNA(y)) {
        warning("NA found in y scale mapping, all were setting to 0")
        y[is.na(y)] <- 0
    }

    return(y)
}


.assign_parent_status <- function(tr, df, variable) {
    yy <- df[[variable]]
    na.idx <- which(is.na(yy))
    if (length(na.idx) > 0) {
        tree <- get.tree(tr)
        nodes <- getNodes_by_postorder(tree)
        for (curNode in nodes) {
            children <- getChild(tree, curNode)
            if (length(children) == 0) {
                next
            }
            idx <- which(is.na(yy[children]))
            if (length(idx) > 0) {
                yy[children[idx]] <- yy[curNode]
            }
        }
    }
    df[, variable] <- yy
    return(df)
}


.assign_child_status <- function(tr, df, variable, yscale_mapping=NULL) {
    yy <- df[[variable]]
    if (!is.null(yscale_mapping)) {
        yy <- yscale_mapping[yy]
    }

    na.idx <- which(is.na(yy))
    if (length(na.idx) > 0) {
        tree <- get.tree(tr)
        nodes <- rev(getNodes_by_postorder(tree))
        for (curNode in nodes) {
            parent <- getParent(tree, curNode)
            if (parent == 0) { ## already reach root
                next
            }
            idx <- which(is.na(yy[parent]))
            if (length(idx) > 0) {
                child <- getChild(tree, parent)
                yy[parent[idx]] <- mean(yy[child], na.rm=TRUE)
            }
        }
    }
    df[, variable] <- yy
    return(df)
}


getYcoord_scale_category <- function(tr, df, yscale, yscale_mapping=NULL, ...) {
    if (is.null(yscale_mapping)) {
        stop("yscale is category variable, user should provide yscale_mapping,
             which is a named vector, to convert yscale to numberical values...")
    }
    if (! is(yscale_mapping, "numeric") ||
        is.null(names(yscale_mapping))) {
        stop("yscale_mapping should be a named numeric vector...")
    }

    if (yscale == "label") {
        yy <- df[[yscale]]
        ii <- which(is.na(yy))
        if (length(ii)) {
            df[ii, yscale] <- df[ii, "node"]
        }
    }

    ## assign to parent status is more prefer...
    df <- .assign_parent_status(tr, df, yscale)
    df <- .assign_child_status(tr, df, yscale, yscale_mapping)

    y <- df[[yscale]]

    if (anyNA(y)) {
        warning("NA found in y scale mapping, all were setting to 0")
        y[is.na(y)] <- 0
    }
    return(y)
}


add_angle_slanted <- function(res) {
    x <- res[["x"]]
    y <- res[["y"]]
    dy <- (y - y[match(res$parent, res$node)]) / diff(range(y))
    dx <- (x - x[match(res$parent, res$node)]) / diff(range(x))
    theta <- atan(dy/dx)
    theta[is.na(theta)] <- 0 ## root node
    res$angle <- theta/pi * 180

    branch.y <- (y[match(res$parent, res$node)] + y)/2
    idx <- is.na(branch.y)
    branch.y[idx] <- y[idx]
    res[, "branch.y"] <- branch.y
    return(res)
}


calculate_branch_mid <- function(res) {
    res$branch <- with(res, (x[match(parent, node)] + x)/2)
    if (!is.null(res$branch.length)) {
        res$branch.length[is.na(res$branch.length)] <- 0
    }
    res$branch[is.na(res$branch)] <- 0
    return(res)
}


re_assign_ycoord_df <- function(df, currentNode) {
    while(anyNA(df$y)) {
        pNode <- with(df, parent[match(currentNode, node)]) %>% unique
        idx <- sapply(pNode, function(i) with(df, all(node[parent == i & parent != node] %in% currentNode)))
        newNode <- pNode[idx]
        ## newNode <- newNode[is.na(df[match(newNode, df$node), "y"])]

        df[match(newNode, df$node), "y"] <- sapply(newNode, function(i) {
            with(df, mean(y[parent == i], na.rm = TRUE))
        })
        traced_node <- as.vector(sapply(newNode, function(i) with(df, node[parent == i])))
        currentNode <- c(currentNode[! currentNode %in% traced_node], newNode)
    }
    return(df)
}

