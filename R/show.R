##' show method for `treedata` instance
##'
##'
##' @name show
##' @docType methods
##' @rdname show-methods
##'
##' @title show method
##' @param object `treedata` object
##' @return print info
##' @importFrom methods show
##' @exportMethod show
##' @usage show(object)
##' @author Guangchuang Yu <https://guangchuangyu.github.io>
setMethod("show", signature(object = "treedata"),
          function(object) {
              print(object)
          })

print_fields <- function(object) {
    fields <- get.fields(object)
    if (length(fields) == 1 && fields == "") {
        return()
    }
    cat("\nwith the following features available:\n")
    ff <- paste0("\t'",paste(fields, collapse="',\t'"), "'.\n")
    cat(fields_wrap(ff))
}

fields_wrap <- function(ff) {
    w <- getOption('width')
    n <- nchar(ff)
    if (w < n) {
        s <- gregexpr("\t", substring(ff, 1, w))[[1]]
        i <- s[length(s)]
        ff2 <- substring(ff, 1:n, 1:n)
        ff2[i] <- '\n\t'
        n <- n+1
        i <- i+1
        ff <- paste0(ff2, collapse='')
        if (w < (n-i)) {
            ff1 <- substring(ff, 1, i)
            ff2 <- substring(ff, i+1, n)
            return(paste0(ff1, fields_wrap(ff2)))
        }
    }
    return(ff)
}

##' @method print treedata
##' @export
print.treedata <- function(x, ..., n = NULL, width = NULL, n_extra = NULL){
    show.data = getOption('show_data_for_treedata', default=TRUE)
    if (show.data){
        print1.treedata(x, n = n, width = width, n_extra = n_extra, ...)
    }else{
        print2.treedata(x, ...)
    }
}

print1.treedata <- function(x, ..., n = NULL, width = NULL, n_extra = NULL){
    if (nrow(x@data) == 0 && nrow(x@extraInfo) == 0){
        n = 10
    }

    annotda <- .extract_annotda.treedata(x)

    formatstr <- annotda %>% format(..., n = n, width = width, n_extra = n_extra)

    fields <- get.fields(x)
    
    if(length(fields)==1 && fields==""){
        fields <- ''
        newheader <- c("\n None available features.")
    }else{
        ff <- paste0("\t'",paste(fields, collapse="',\t'"), "'.\n")
        fields <- fields_wrap(ff)
        newheader <- c("\nwith the following features available:", fields)
    }

    msg <- .internal_print.treedata_msg(x) %>%
           writeLines()

    print.phylo(as.phylo(x))

    formatstr[1] <- gsub("(A tibble:)", "The associated data tibble abstraction:", formatstr[1])
    formatstr %<>% append(pillar::style_subtle("# The 'node', 'label' and 'isTip' are from the phylo tree."), 
                          after=1)
    newheader %>%
      append(formatstr) %>%
      writeLines()

    invisible(x)
}

#' @importFrom ape print.phylo
print2.treedata <- function(x, ...) {
    msg <- .internal_print.treedata_msg(x)
    writeLines(msg)
    print.phylo(as.phylo(x))
    print_fields(x)
}

.internal_print.treedata_msg <- function(x){
    msg <- "'treedata' S4 object"
    files <- x@file
    files <- files[files != ""]
    if (length(files)) {
        ff <- paste(files, collapse="',\n\t'")
        msg <- paste0(msg,
                      " that stored information of\n\t",
                      "'", ff)
    }

    msg <- paste0(msg, "'.\n")
    msg <- c(msg, "...@ phylo:")
    
    return(msg) 
}

.internal_add_isTip <- function(x){
    x %<>% mutate(isTip=ifelse(!.data$node %in% .data$parent, TRUE, FALSE))
    return(x)
}
