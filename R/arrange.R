#' @importFrom dplyr arrange

#' @exportS3Method
arrange.DFrame <- function(DF, ..., .by_group=FALSE) {
  loc <- .arrange_loc(to_df(DF), ..., .by_group=.by_group)
  dplyr_row_slice(DF, loc)
}

.arrange_loc <- function(.data, ..., .by_group=FALSE) {
  dots <- enquos(...)
  if (.by_group) {
    dots <- c(quos(!!!groups(.data)), dots)
  }
  dplyr:::arrange_rows(.data, dots)
}

arrange_row <- function(se, ..., .by_group=FALSE) {
  df <- to_df(rowData(se))
  loc <- with_bindings(
    .arrange_loc(df, !!!exprs(...), .by_group = .by_group),
    !!!assays(se)@listData, !!!se@colData@listData
  )
  dplyr_row_slice(se, loc, .dim="row")
}

arrange_col <- function(se, ..., .by_group=FALSE) {
  df <- to_df(colData(se))
  loc <- with_bindings(
    .arrange_loc(df, !!!exprs(...), .by_group = .by_group),
    !!!assays(se)@listData, !!!se@elementMetadata@listData
  )
  dplyr_row_slice(se, loc, .dim="col")
}

#' @exportS3Method
arrange.SummarizedExperiment <- function(se, ..., .by_group=FALSE, .dim=c("row", "col")) {
  dim <- match.arg(.dim)
  # if (.dim == 'row') {
  #   arrange_row(se, ..., .by_group = .by_group)
  # } else {
  #   arrange_col(se, ..., .by_group = .by_group)
  # }
  mask <- prepare_data_mask(se, .dim)
  loc <- with_bindings(
    .arrange_loc(to_df(mask$DF), !!!exprs(...), .by_group = .by_group),
    !!!mask$mask
  )
  dplyr_row_slice(se, loc, .dim=.dim)
}
