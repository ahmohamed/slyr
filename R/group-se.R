#' @exportS3Method
group_by.SummarizedExperiment <- function(se, ..., .dim) {
  stopifnot(!missing(.dim))
  .dim = match.arg(.dim, c('row', 'col'))
  if (.dim == 'row') {
    rowData(se) <- with_bindings(
      group_by(rowData(se), ...),
      !!!assays(se)@listData, !!!se@colData@listData
    )
  }
  if (.dim == 'col') {
    colData(se) <- with_bindings(
      group_by(colData(se), ...),
      !!!assays(se)@listData, !!!se@elementMetadata@listData
    )
  }
  se
}

#' @exportS3Method
group_data.SummarizedExperiment <- function(se) {
  list(
    row=group_data(rowData(se)),
    col=group_data(colData(se))
  )
}

#' @exportS3Method
group_vars.SummarizedExperiment <- function(se) {
  list(
    row=group_vars(rowData(se)),
    col=group_vars(colData(se))
  )
}

#' @exportS3Method
tbl_vars.SummarizedExperiment <- function(se) {
  list(
    row=tbl_vars(rowData(se)),
    col=tbl_vars(colData(se))
  )
}


#' @exportS3Method
groups.SummarizedExperiment <- function(se) {
  list(
    row=groups(rowData(se)),
    col=groups(colData(se))
  )
}


#' @exportS3Method
group_keys.SummarizedExperiment <- function(se) {
  list(
    row=group_keys(rowData(se)),
    col=group_keys(colData(se))
  )
}

# Will work as is. Not an S3 method
#' @export
dplyr::group_rows

# #' @exportS3Method
# group_cols.SummarizedExperiment <- dplyr:::group_cols

#' @exportS3Method
group_indices.SummarizedExperiment <- function(se) {
  list(
    row=group_indices(rowData(se)),
    col=group_indices(colData(se))
  )
}


#' @exportS3Method
ungroup.SummarizedExperiment <- function(se, ..., .dim='all') {
  if (.dim == 'all') {
    .dim = c('row', 'col')
  }
  .dim = match.arg(.dim, c('row', 'col'), several.ok = TRUE)

  if ('row' %in% .dim) {
    rowData(se) <- ungroup(rowData(se), ...)
  }
  if ('col' %in% .dim) {
    colData(se) <- ungroup(colData(se), ...)
  }
  se
}

#' @export
dplyr::add_tally
