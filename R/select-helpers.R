#' @importFrom dplyr n
#' @importFrom tidyselect everything one_of any_of all_of contains matches starts_with ends_with

#' @export
dplyr::n

#' @export
tidyselect::everything

#' @export
tidyselect::all_of

#' @export
tidyselect::any_of

#' @export
tidyselect::one_of

#' @export
tidyselect::contains

#' @export
tidyselect::matches

#' @export
tidyselect::starts_with

#' @export
tidyselect::ends_with

#' @export
all_assay <- function(.se) {
  .top <- dplyr:::data_mask_top(rlang::caller_env())
  dplyr::one_of(assayNames(.top$.))
}

#' @export
all_row <- function(.se) {
  .top <- dplyr:::data_mask_top(rlang::caller_env())
  dplyr::one_of(colnames(rowData(.top$.)))
}

#' @export
all_col <- function() {
  .top <- dplyr:::data_mask_top(rlang::caller_env())
  dplyr::one_of(colnames(colData(.top$.)))
}
