#' @method groupClade tbl_tree
#' @export
groupClade.tbl_tree <- function(.data, .node,
                                group_name = "group",
                                overlap = "overwrite", ...) {

    overlap <- match.arg(overlap, c("origin", "overwrite", "abandon"))
    valid.tbl_tree(.data)

    n <- nrow(.data)
    foc <- rep(0, n)
    if (length(.node) == 1) {
        ids <- c(.node, offspring(.data, .node)$node)
        foc[ids] <- 1
        .data[[group_name]] <- factor(foc[match(1:n, .data$node)])
        return(.data)
    }

    ## One pass for all nodes instead of one offspring() call per node:
    ## the latter is O(n^2) in the number of clades.
    bulk <- offspring_bulk(.data, nodes = .node)

    for (i in seq_along(.node)) {
        hit <- c(.node[i], bulk[[i]])

        if (overlap == "origin") {
            sn <- hit[is.na(foc[hit]) | foc[hit] == 0]
        } else if (overlap == "abandon") {
            idx <- !is.na(foc[hit]) & foc[hit] != 0
            foc[hit[idx]] <- NA
            sn <- hit[!idx]
        } else {
            sn <- hit
        }

        if (length(sn) > 0) {
            if (is.null(names(.node)[i])) {
                foc[sn] <- max(suppressWarnings(as.numeric(foc)), na.rm=TRUE) + 1
            } else {
                foc[sn] <- names(.node)[i]
            }
        }
    }
    .data[[group_name]] <- factor(foc[match(1:n, .data$node)])
    return(.data)
}
