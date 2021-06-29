#' @importFrom dplyr slice slice_head slice_tail slice_sample slice_min slice_max
#' @importFrom dplyr dplyr_row_slice

#' @exportS3Method
slice.DFrame <- function(DF, ..., .preserve = FALSE) {
  loc <- dplyr:::slice_rows(to_df(DF), ..., .preserve = FALSE)
  dplyr_row_slice(DF, loc)
}


#' @exportS3Method
slice_head.DFrame <- dplyr:::slice_head.data.frame

#' @exportS3Method
slice_tail.DFrame <- dplyr:::slice_tail.data.frame

#' @exportS3Method
slice_sample.DFrame <- dplyr:::slice_sample.data.frame

#' @exportS3Method
slice_min.DFrame <- dplyr:::slice_min.data.frame

#' @exportS3Method
slice_max.DFrame <- dplyr:::slice_max.data.frame

# modified from dplyr_row_slice.grouped_df
dplyr_row_slice.DFrame <- function(data, i, ..., preserve = FALSE) {
  out <- data[i, ]
  if (is.null(attr(data, 'groups'))) {
    return(out)
  }
  # Index into group_indices, then use that to restore the grouping structure
  groups <- group_data(data)
  new_id <- vctrs::vec_slice(group_indices(data), i)
  new_grps <- vctrs::vec_group_loc(new_id)

  rows <- rep(vctrs::list_of(integer()), length.out = nrow(groups))
  rows[new_grps$key] <- new_grps$loc
  groups$.rows <- rows

  if (!preserve) {
    groups <- dplyr:::group_data_trim(groups)
  }
  attr(out, "groups") <- groups
  out
}

#' @exportS3Method
slice.DFrameInt <- function(DF, ..., .preserve = FALSE) {
  browser()
  loc <- with_bindings(
    dplyr:::slice_rows(DF, ..., .preserve = FALSE),
    !!!attr(DF, "data_mask")
  )
}

.slice_se_adapter <- function(slice_fn) {
  function(se, ..., .dim) {
    stopifnot(!missing(.dim))
    .dim = match.arg(.dim, c('row', 'col'))
    if (.dim == 'row') {
      DF <- rowData(se)
      data_mask <- c(assays(se)@listData, colData(se)@listData)
    } else {
      DF <- colData(se)
      data_mask <- c(assays(se)@listData, rowData(se)@listData)
    }

    DF <- to_df(DF)
    DF$..idx = 1:nrow(DF)

    # We have to use do.call here: cannot use !!! at top level
    args = append(list(DF), exprs(...))
    loc <- with_bindings(do.call(slice_fn, args)$..idx, !!!data_mask)
    dplyr_row_slice(se, loc, .dim=.dim)
  }
}

dplyr_row_slice.SummarizedExperiment <- function(data, i, ..., .dim, preserve = FALSE) {
  stopifnot(!missing(.dim))
  .dim = match.arg(.dim, c('row', 'col'))

  if(.dim == 'row') {
    DF_sliced <- dplyr_row_slice(rowData(data), i, ..., preserve=preserve)
    data <- data[i, ]
    rowData(data) <- DF_sliced
  } else {
    DF_sliced <- dplyr_row_slice(colData(data), i, ..., preserve=preserve)
    data <- data[, i]
    colData(data) <- DF_sliced
  }
  data
}

#' @exportS3Method
slice.SummarizedExperiment <- .slice_se_adapter(slice)

# slice.SummarizedExperiment <- function(se, ..., .dim, .preserve = FALSE) {
#   stopifnot(!missing(.dim))
#   .dim = match.arg(.dim, c('row', 'col'))
#   if (.dim == 'row') {
#     DF <- rowData(se)
#   } else {
#     DF <- colData(se)
#   }
#
#   loc <- dplyr:::slice_rows(to_df(DF), ..., .preserve = FALSE)
#   DF_sliced <- dplyr_row_slice(DF, loc)
#
#   if(.dim == 'row') {
#     se <- se[loc, ]
#     rowData(se) <- DF_sliced
#   } else {
#     se <- se[, loc]
#     colData(se) <- DF_sliced
#   }
#   se
# }


#' @exportS3Method
slice_head.SummarizedExperiment <- .slice_se_adapter(slice_head)

#' @exportS3Method
slice_tail.SummarizedExperiment <- .slice_se_adapter(slice_tail)

#' @exportS3Method
slice_sample.SummarizedExperiment <- .slice_se_adapter(slice_sample)

#' @exportS3Method
slice_min.SummarizedExperiment <- .slice_se_adapter(slice_min)

#' @exportS3Method
slice_max.SummarizedExperiment <- .slice_se_adapter(slice_max)


# a <- function(.se, ...) {
#   with_bindings(
#     slice(rowData(.se), !!!exprs(...)),
#     !!!c(assays(.se)@listData, colData(.se)@listData)
#   )
# }
# amax <- function(.se, ...) {
#   eval_tidy(
#     slice_max(rowData(.se), ...),
#     as_data_mask(c(assays(.se)@listData, colData(.se)@listData))
#   )
# }
# a(.se, which(rowSums(A1)>1000))
#
# amax(.se, rowSums(A1), n=3)
