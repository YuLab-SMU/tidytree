##' @method rename treedata
##' @importFrom tidyselect eval_select
##' @export
rename.treedata <- function(.data, ...){
    dat <- .data %>% .extract_annotda.treedata()
    
    cols <- eval_select(rlang::expr(c(...)), dat)

    loc <- check_names_from_phylo(x=dat, recol=cols)
    clnames <- colnames(dat)

    .data@data <- .update.td.rename(x=.data@data, loc=loc, clnames=clnames)

    .data@extraInfo <- .update.td.rename(x=.data@extraInfo, loc=loc, clnames=clnames)

    return(.data)
}

#' @param x the data before rename
#' @param recol the column will be renamed
#' @noRd
check_names_from_phylo <- function(x, recol){
    clnm <- colnames(x)
    renm <- clnm[recol]
    if (any(renm %in% c("node", "label", "isTip"))){
        warning("The 'node', 'label' and 'isTip' do not be renamed !")
        ind <- seq_len(length(recol))
        names(ind) <- renm
        ind <- ind[!names(ind) %in% c("node", "label", "isTip")]
        recol <- recol[unname(ind)]
    }
    return(recol)
}

#' @noRd
.update.td.rename <- function(x, loc, clnames){
    clnmda <- colnames(x)
    loc <- sort(loc) 
    ind.da1 <- which(clnames[loc] %in% clnmda)
    ind.da2 <- which(clnmda %in% clnames[loc])
    clnmda[ind.da2] <- names(loc)[ind.da1]

    colnames(x) <- clnmda
    return(x)
}
