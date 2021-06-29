#' @importFrom tibble view
#' @importFrom SummarizedExperiment colData rowData assays assay assayNames
#' @importFrom S4Vectors DataFrame

copy_groups <- function(to, from) {
  groups <- attr(from, "groups")
  attr(to, "groups") <- groups
  to
}

to_DFrame <- function(df) {
  copy_groups(DataFrame(df), df)
}

to_df <- function(DF) {
  df <- copy_groups(as.data.frame(DF), DF)
  if(!is.null(attr(df, "groups")))
    class(df) <- c("grouped_df", "tbl_df", "tbl", class(df))
  df
}

se_data_idx <- function(se) {
  c(
    rlang::rep_named(colnames(rowData(se)), "row"),
    rlang::rep_named(colnames(colData(se)), "col"),
    rlang::rep_named(assayNames(se), "assay")
  )
}

# se_data_idx <- function(se) {
#   list(
#     type=c(
#       rlang::rep_named(colnames(rowData(se)), "row"),
#       rlang::rep_named(colnames(colData(se)), "col"),
#       rlang::rep_named(assayNames(se), "assay")
#     ),
#     idx=c(
#       seq_along(colnames(rowData(se))),
#       seq_along(colnames(colData(se))),
#       seq_along(assayNames(se))
#     )
#   )
# }
se_data_mask <- function(se) {
  c(
    rowData(se)@listData,
    colData(se)@listData,
    assays(se)@listData
  )
}
