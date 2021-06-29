expect_cols_equal <- function(a, b, cols=NULL) {
  if (is.null(cols)) {
    cols <- colnames(a)
  }
  if (is.null(names(cols))) {
    names(cols) <- cols
  }

  for (i in names(cols)) {
    expect_equal(a[[i]], b[[ cols[[i]] ]])
  }
}
