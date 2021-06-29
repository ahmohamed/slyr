#' @importFrom dplyr filter

#' @exportS3Method
filter.DFrame <- function(DF, ..., .preserve=FALSE) {
  loc <- dplyr:::filter_rows(to_df(DF), ..., caller_env = caller_env())
  dplyr_row_slice(DF, loc)
}

filter_col <- function(se, ...) {
  dplyr:::filter_rows(to_df(colData(se)), ..., caller_env = caller_env())
}

filter_row <- function(se, ...) {
  dplyr:::filter_rows(to_df(rowData(se)), ..., caller_env = current_env())
}

#' @exportS3Method
filter.SummarizedExperiment <- function(se, ..., .preserve=FALSE, .dim=c('row', 'col')) {
  .dim <- match.arg(.dim)
  # if (.dim == 'row') {
  #   loc <- with_bindings(
  #     filter_row(se, !!!exprs(...)),
  #     !!!assays(se)@listData, !!!se@colData@listData
  #   )
  # } else {
  #   loc <- with_bindings(
  #     filter_col(se, !!!exprs(...)),
  #     !!!assays(se)@listData, !!!se@elementMetadata@listData
  #   )
  # }
  mask <- prepare_data_mask(se, .dim)
  loc <- with_bindings(
    dplyr:::filter_rows(to_df(mask$DF), !!!exprs(...), caller_env = caller_env()),
    !!!mask$mask
  )
  dplyr_row_slice(se, loc, .dim=.dim)
}
