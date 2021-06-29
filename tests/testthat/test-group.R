data("lipids")
clipids <- colData(lipids)
rlipids <- rowData(lipids)

expect_groups_equal <- function(DF, df) {
  expect_equal(group_data(DF), group_data(df))
  expect_equal(group_rows(DF), group_rows(df))
  expect_equal(group_vars(DF), group_vars(df))
  expect_equal(groups(DF), groups(df))
  expect_equal(group_keys(DF), group_keys(df))
}

test_that("group_data functions work", {
  # Before grouping
  DF <- clipids
  df <- clipids %>% dplyr::as_tibble()
  expect_groups_equal(DF, df)

  # Single group
  DF <- clipids %>% group_by(Diet)
  df <- clipids %>% as.data.frame() %>% group_by(Diet)
  expect_groups_equal(DF, df)

  # Multiple group
  DF <- clipids %>% group_by(Diet, BileAcid)
  df <- clipids %>% as.data.frame() %>% group_by(Diet, BileAcid)
  expect_groups_equal(DF, df)

  # no groups
  DF <- clipids
  df <- clipids %>% dplyr::as_tibble()
  expect_groups_equal(DF, df)

  # ungroup all
  DF <- clipids %>% group_by(Diet, BileAcid) %>% ungroup()
  df <- clipids %>% as.data.frame() %>% group_by(Diet, BileAcid) %>% ungroup()
  expect_groups_equal(DF, df)

  # ungroup some vars
  DF <- clipids %>% group_by(Diet, BileAcid) %>% ungroup(Diet)
  df <- clipids %>% as.data.frame() %>% group_by(Diet, BileAcid) %>% ungroup(Diet)
  expect_groups_equal(DF, df)
})
