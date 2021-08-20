##' @rdname get.fields-methods
##' @aliases get.fields,treedata
##' @exportMethod get.fields
setMethod("get.fields", signature(object = "treedata"),
          function(object) {
              get.fields.treedata(object)
          })


get.fields.treedata <- function(object) {
    fields1 <- get.fields.data(object)
    fields2 <- get.fields.extraInfo(object)
    return(c(fields1, fields2))
}

get.fields.data <- function(object){
    if (nrow(object@data) > 0) {
        fields <- colnames(object@data)
        fields <- fields[fields != "node"]
    } else {
        fields <- ""
    }
    return(fields)
}

get.fields.extraInfo <- function(object){
    extraInfo <- object@extraInfo
    if (nrow(extraInfo) > 0) {
        cn <- colnames(extraInfo)
        i <- match(c("x", "y", "isTip", "node", "parent", "label", "branch", "branch.length"), cn)
        i <- i[!is.na(i)]
        fields <- cn[-i]
        return(fields)
    }else{
        return(character(0))
    }
}
