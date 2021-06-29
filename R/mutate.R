#' @importFrom dplyr mutate

.mutate_dframe <- forDFrame(dplyr::mutate)

#' @exportS3Method
mutate.DFrame <- function(DF, ...) {
  out <-.mutate_dframe(DF, ...)
  rownames(out) <- rownames(DF)
  out
}


#' @exportS3Method
mutate.SummarizedExperiment <- function(se, ..., .dim=c('row', 'col')) {
  .dim <- match.arg(.dim)
  if (.dim == 'row') {
    rowData(se) <- with_bindings(
      mutate(rowData(se), !!!exprs(...)),
      !!!assays(se)@listData, !!!se@colData@listData
    )
  } else {
    colData(se) <- with_bindings(
      mutate(colData(se), !!!exprs(...)),
      !!!assays(se)@listData, !!!se@elementMetadata@listData
    )
  }
  se
}
