data("lipids")
rdata <- rowData(lipids)
rdf <- as.data.frame(rdata)

cdata <- colData(lipids)
cdf <- as.data.frame(cdata)

test_that("converts from DFrame to df retaining rownames", {
  df <- to_df(cdata)
  expect_equal(rownames(df), rownames(cdata))
  lapply(colnames(cdata), function(c) expect_equal(df[[c]], cdata[[c]]))

  single_col_dframe <- cdata[, 1, drop=FALSE]
  df <- to_df(single_col_dframe)
  expect_equal(rownames(df), rownames(single_col_dframe))
  lapply(colnames(single_col_dframe), function(c) expect_equal(df[[c]], single_col_dframe[[c]]))

})

test_that("double conversion yields original object", {
  expect_equal(cdf %>% to_DFrame() %>% to_df(), cdf)
  expect_equal(cdata %>% to_df() %>% to_DFrame(), cdata)
})

test_that("double conversion yields original object in DFs withn no or single column", {
  expect_equal(cdf[, 1, drop=FALSE] %>% to_DFrame() %>% to_df(), cdf[, 1, drop=FALSE])
  expect_equal(cdata[, 1, drop=FALSE] %>% to_df() %>% to_DFrame(), cdata[, 1, drop=FALSE])
})

test_that("converts from DFrame to df retaining grouping vars", {
  grouped <- cdf %>% group_by(Diet, BileAcid)
  converted <- grouped %>% to_DFrame() %>% to_df()
  expect_equal(
    grouped %>% dplyr::summarise_all(dplyr::first),
    converted %>% dplyr::summarise_all(dplyr::first)
  )
})
