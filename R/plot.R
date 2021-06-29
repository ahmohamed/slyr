#' @importFrom ggplot2 fortify

#' @exportS3Method
fortify.DFrame <- to_df

#' @exportS3Method
fortify.SummarizedExperiment <- function(model, data, ...) {
  .to_long(model)
}

.to_long <- function(se) {
  if(is.null(rownames(se))) {
    rownames(se) <- as.character(1:nrow(se))
  }
  long_assays <- lapply(assayNames(se), function(name) {
    .assay_to_long(assay(se, name), name)
  })
  Reduce(function(x, y) dplyr::left_join(x, y, by=c("..row", "..col")), long_assays) %>%
    dplyr::left_join(
      colData(se) %>% to_df() %>% tibble::rownames_to_column("..col"),
      by = "..col"
    ) %>%
    dplyr::left_join(
      rowData(se) %>% to_df() %>% tibble::rownames_to_column("..row"),
      by = "..row"
    )
}

.assay_to_long <- function(.assay, name) {
  as.data.frame(.assay) %>% tibble::rownames_to_column("..row") %>%
    tidyr::gather(key = "..col", value = !!name, -..row)
}
