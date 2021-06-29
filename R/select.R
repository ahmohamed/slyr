#' @import rlang
#' @importFrom dplyr select rename
#' @importFrom SummarizedExperiment colData<- rowData<- assayNames<- assays<-

#' @exportS3Method
select.DFrame <- function(DF, ...) {
  loc <- tidyselect::eval_select(expr(c(...)), DF@listData)
  .select_cols(DF, loc)
}

#' @exportS3Method
rename.DFrame <- function(DF, ...) {
  loc <- tidyselect::eval_rename(expr(c(...)), DF@listData)
  names <- names(DF)
  names[loc] <- names(loc)
  .set_names(DF, names)
}


#' @exportS3Method
rename.SummarizedExperiment <- function(se, ..., dim='auto') {
  .dim = match.arg(.dim, c('auto', 'row', 'col', 'assay'), several.ok = TRUE)
  se_list <- se_data_idx(se)

  if (.dim == 'auto' && any(duplicated(names(se_list)))) {
    stop("Can't use select function with .dim='auto' because there are duplicates in column names.")
  }
  if (.dim == 'auto') {
    .dim = c('row', 'col', 'assay')
  }
  else {
    se_list <- se_list[se_list %in% .dim]
  }
  loc <- tidyselect::eval_rename(expr(c(...)), se_list)
  names <- names(se_list)
  names[loc] <- names(loc)

  if ('col' %in% .dim) {
    colData(se) <- .set_names(colData(se), names[se_list == 'col'])
  }
  if ('row' %in% .dim) {
    rowData(se) <- .set_names(rowlData(se), names[se_list == 'row'])
  }
  if ('assay' %in% .dim) {
    assayNames(se) <- names[se_list == 'assay']
  }
  se
}

#' @exportS3Method
select.SummarizedExperiment <- function(se, ..., .dim='auto') {
  loc <- .select_loc_se(se, ..., .dim=.dim)
  if ('row' %in% names(loc)) {
    rowData(se) <- .select_cols(rowData(se), loc$row)
  }
  if ('col' %in% names(loc)) {
    colData(se) <- .select_cols(colData(se), loc$col)
  }
  if ('assay' %in% names(loc)) {
    assays(se) <- setNames(assays(se)[loc$assay], names(loc$assay))
  }

  se
}

.set_names <- function (DF, value) {
  if (is.null(attr(DF, "groups"))) {
      return(setNames(DF, value))
    }
  groups <- group_data(DF)
  group_loc <- match(intersect(names(groups), names(DF)), names(DF))
  group_names <- c(value[group_loc], ".rows")
  if (!identical(group_names, names(groups))) {
    names(groups) <- c(value[group_loc], ".rows")
  }
  names(DF) <- value
  attr(DF, "groups") <- groups
  DF
}

.select_cols <- function(DF, loc) {
  loc <- dplyr:::ensure_group_vars(loc, DF, notify = TRUE)
  .set_names(DF[, loc, drop=FALSE], names(loc))
}

.select_loc_se <- function(se, ..., .dim) {
  .dim = match.arg(.dim, c('auto', 'row', 'col', 'assay'), several.ok = TRUE)

  data_mask <- se_data_mask(se)
  se_list <- se_data_idx(se)

  if (.dim == 'auto' && any(duplicated(names(se_list)))) {
    stop("Can't use select function with .dim='auto' because there are duplicates in column names.")
  }
  if (.dim == 'auto') {
    .dim = c('row', 'col', 'assay')
  }
  else {
    data_mask <- data_mask[se_list %in% .dim]
    se_list <- se_list[se_list %in% .dim]
  }

  loc <- tidyselect::eval_select(expr(c(...)), data_mask)
  row_loc <- loc[se_list[loc] == "row"]
  col_loc <- loc[se_list[loc] == "col"] - sum(se_list == "row")
  assay_loc <- loc[se_list[loc] == "assay"] - sum(se_list %in% c("col", "row"))
  list(row=row_loc, col=col_loc, assay=assay_loc)[.dim]
}
