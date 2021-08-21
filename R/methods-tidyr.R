#' @method unnest treedata
#' @export
unnest.treedata <- function(data, 
                            cols, ..., 
                            keep_empty = FALSE, 
                            ptype = NULL, 
                            names_sep = NULL, 
                            names_repair = "check_unique"){
    tbl_df_returned_message %>% 
      pillar::style_subtle() %>% 
      writeLines()
    cols <- rlang::enquo(cols)
    data <- .extract_annotda.treedata(data)
    data <- unnest(data, !!cols, ..., keep_empty=keep_empty,
                   ptype=ptype, names_sep=names_sep, names_repair=names_repair)
    return(data)
}
