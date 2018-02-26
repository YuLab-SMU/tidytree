##' show method for \code{treedata} instance
##'
##'
##' @name show
##' @docType methods
##' @rdname show-methods
##'
##' @title show method
##' @param object \code{treedata} object
##' @return print info
##' @importFrom methods show
##' @exportMethod show
##' @usage show(object)
##' @author Guangchuang Yu \url{https://guangchuangyu.github.io}
setMethod("show", signature(object = "treedata"),
          function(object) {
              print.treedata(object)
          })

##' @method print treedata
##' @importFrom ape print.phylo
##' @export
print.treedata <- function(x, ...) {
    msg <- "'treedata' S4 object"
    files <- x@file
    files <- files[files != ""]
    if (length(files)) {
        ff <- paste(files, collapse="',\n\t'")
        msg <- paste0(msg,
                      " that stored information of\n\t",
                      "'", ff)
    }

    msg <- paste0(msg, "'.\n\n")
    cat(msg)

    cat("...@ phylo: ")
    print.phylo(as.phylo(x))
    print_fields(x)
}


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
