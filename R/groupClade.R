##' @method groupClade tbl_tree
##' @export
##' @importFrom treeio groupClade
groupClade.tbl_tree <- function(.data, .node,
                                group_name = "group",
                                overlap = "overwrite", ...) {

    overlap <- match.arg(overlap, c("origin", "overwrite", "abandon"))

    n <- nrow(.data)
    foc <- rep(0, n)
    if (length(.node) == 1) {
        ids <- c(.node, offspring(.data, .node)$node)
        foc[ids] <- 1
        .data[[group_name]] <- factor(foc[match(1:n, .data$node)])
        return(.data)
    }

    for (i in seq_along(.node)) {
        hit <- c(.node[i], offspring(.data, .node[i])$node)

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
