##' @method groupOTU tbl_tree
##' @export
##' @importFrom treeio groupOTU
##' @importFrom methods is
groupOTU.tbl_tree <- function(.data, .node,
                              group_name = "group",
                              ...) {
    valid.tbl_tree(.data)
    .data[[group_name]] <- NULL
    if ( is(.node, "list") ) {
        for (i in seq_along(.node)) {
            .data <- groupOTU.tbl_tree_item(.data, .node[[i]],
                                            names(.node)[i],
                                            group_name = group_name,
                                            ...)
        }
    } else {
        .data <- groupOTU.tbl_tree_item(.data, .node,
                                        group_name = group_name,
                                        ...)
    }

    .data[[group_name]] <- factor(.data[[group_name]])
    return(.data)
}

##' @importFrom dplyr group_by_
groupOTU.tbl_tree_item <- function(.data, .node,
                                   focus_label = NULL,
                                   group_name,
                                   overlap="overwrite",
                                   connect = FALSE) {

    ## see https://groups.google.com/forum/#!msg/bioc-ggtree/Q4LnwoTf1DM/yEe95OFfCwAJ
    ## for connect parameter

    overlap <- match.arg(overlap, c("origin", "overwrite", "abandon"))

    focus <- .node
    if (is.character(focus)) {
        focus <- filter_(.data, ~(label %in% .node))$node
    }

    n <- nrow(.data)

    if (is.null(.data[[group_name]])) {
        foc <- rep(0, n)
    } else {
        foc <- .data[[group_name]]
    }

    g <- max(suppressWarnings(as.numeric(foc)), na.rm=TRUE) + 1
    if (is.null(focus_label)) {
        focus_label <- g
    }

    anc <- lapply(focus, function(.node) sort(ancestor(.data, .node)$node))
    ll <- min(sapply(anc, length))
    i <- 2L
    repeat {
        if ( i > ll) {
            break
        }

        x <- unique(unlist(lapply(anc, "[", i)))
        if (length(x) != 1)
            break
        i <- i + 1L
    }
    d <- -(1:(i - 1L))
    x <- unique(unlist(lapply(anc, function(x) x[d])))
    hit <- unique(c(anc[[1]][i-1L], x, focus))

    if (overlap == "origin") {
        sn <- hit[is.na(foc[hit]) | foc[hit] == 0]
    } else if (overlap == "abandon") {
        idx <- !is.na(foc[hit]) & foc[hit] != 0
        foc[hit[idx]] <- NA
        sn <- hit[!idx]
    } else {
        sn <- hit
    }

    if (length(sn) > 0 && connect) {
        y <- filter_(.data, ~ node %in% sn) %>% group_by_(~parent)  %>% summarize(degree = n())
        if ( sum(y$degree > 1) == 1 ) {
            sn <- focus
        }
    }

    if (length(sn)) {
        foc[sn] <- focus_label
    }

    .data[[group_name]] <- foc[match(1:n, .data$node)]
    return(.data)
}


