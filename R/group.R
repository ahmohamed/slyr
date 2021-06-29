#' @importFrom dplyr group_by ungroup
#' @importFrom dplyr group_data group_vars group_rows group_indices group_keys groups cur_group_id
#' @importFrom dplyr tbl_vars add_tally

#' @exportS3Method
group_by.DFrame <- function(DF, ...) {
  copy_groups(DF, dplyr::group_by(to_df(DF), ...))
}

#' @exportS3Method
group_data.DFrame <- function(DF) {
  attr(DF, "groups") %||% dplyr::as_tibble(dplyr:::group_data.data.frame(DF))
}

#' @exportS3Method
group_vars.DFrame <- dplyr:::group_vars.data.frame

#' @exportS3Method
tbl_vars.DFrame <- dplyr:::tbl_vars.data.frame


#' @exportS3Method
groups.DFrame <- dplyr:::groups.data.frame


#' @exportS3Method
group_keys.DFrame <- dplyr:::group_keys.data.frame

# Will work as is. Not an S3 method
#' @export
dplyr::group_rows

# #' @exportS3Method
# group_cols.DFrame <- dplyr:::group_cols

#' @exportS3Method
group_indices.DFrame <- function(.data, ...) {
  group_indices(to_df(.data))
}


#' @exportS3Method
ungroup.DFrame <- function(x, ...) {
  if (missing(...)) {
    out <- x
    attr(out, "groups") <- NULL
    out
  } else {
    dplyr:::ungroup.grouped_df(x, ...)
  }
}

#' @export
dplyr::add_tally

