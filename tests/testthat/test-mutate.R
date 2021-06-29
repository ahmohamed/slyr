data("lipids")
clipids <- colData(lipids)
rlipids <- rowData(lipids)

test_that("mutates a DFrame by direct assignment", {
  DF <- clipids %>% mutate(group2=group, rown = 1:n())
  expect_equal(colnames(DF), c("group", "Diet", "BileAcid", "group2", "rown"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(
    clipids, DF,
    c(group="group", group="group2", Diet="Diet", BileAcid="BileAcid")
  )
  expect_equal(DF$rown, 1:nrow(clipids))

  #single column
  DF <- clipids %>% select() %>% mutate(rown = 1:n())
  expect_equal(colnames(DF), c("rown"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(DF$rown, 1:nrow(clipids))

  #no columns
  DF <- clipids %>% select() %>% mutate()
  expect_equal(colnames(DF), character())
  expect_equal(rownames(DF), rownames(clipids))

  # overwrite column (doesn't change order)
  DF <- clipids %>% mutate(group=BileAcid)
  expect_equal(colnames(DF),  c("group", "Diet", "BileAcid"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(
    DF, clipids,
    c(group="BileAcid", Diet="Diet", BileAcid="BileAcid")
  )

  #vector recycling
  DF <- clipids %>% mutate(group=1, bool_col=TRUE)
  expect_equal(colnames(DF),  c("group", "Diet", "BileAcid", "bool_col"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(clipids, DF,c("Diet", "BileAcid"))
  expect_equal(DF$group, rep(1, nrow(DF)))
  expect_equal(DF$bool_col, rep(TRUE, nrow(DF)))
})

test_that("mutates a DFrame by column operations", {
  n_rows <- nrow(clipids)
  row_numbers <- 1:n_rows

  # pasting 2 columns
  DF <- clipids %>% mutate(group_diet=paste(group, Diet))
  expect_equal(colnames(DF), c("group", "Diet", "BileAcid", "group_diet"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(clipids, DF, c("group", "Diet", "BileAcid"))
  expect_equal(DF$group_diet, paste(clipids$group, clipids$Diet))


  # pasting 2 columns and overwriting one of them
  DF <- clipids %>% mutate(group=paste(group, Diet))
  expect_equal(colnames(DF), c("group", "Diet", "BileAcid"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(clipids, DF, c("Diet", "BileAcid"))
  expect_equal(DF$group, paste(clipids$group, clipids$Diet))

  # create a new column & use it in the same expr
  DF <- clipids %>% mutate(rown = 1:n(), group=paste(group, rown))
  expect_equal(colnames(DF), c("group", "Diet", "BileAcid", "rown"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(DF$rown, 1:n_rows)
  expect_equal(DF$group, paste(clipids$group, 1:n_rows))

  # type conversion mutations
  DF <- clipids %>% mutate(BileAcid=as.factor(BileAcid))
  expect_equal(colnames(DF), c("group", "Diet", "BileAcid"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(lapply(DF, class), list(group="character", Diet="character", BileAcid="factor"))
  expect_equal(levels(DF$BileAcid), c("DCA", "QC", "water"))


  # fct_* mutations
  DF <- clipids %>% mutate(BileAcid=forcats::fct_inorder(BileAcid))
  expect_equal(colnames(DF), c("group", "Diet", "BileAcid"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(lapply(DF, class), list(group="character", Diet="character", BileAcid="factor"))
  expect_equal(levels(DF$BileAcid), c("water", "DCA", "QC"))

  # multiple column mutations with across
  DF <- clipids %>% mutate(across(group:BileAcid, as.factor))
  expect_equal(colnames(DF), c("group", "Diet", "BileAcid"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(lapply(DF, class), list(group="factor", Diet="factor", BileAcid="factor"))

  # list columns
  list_val = list(val=1, val2=2)
  DF <- clipids %>% mutate(list_col=list(list_val))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(lapply(DF, class), list(group="character", Diet="character", BileAcid="character", list_col="list"))
  expect_equal(DF$list_col[[1]], list_val)
})

test_that("mutates a grouped DFrame", {
  clipids <- clipids %>% filter(group != "QC") %>% mutate(Replicate=1:n())
  n_rows <- nrow(clipids)

  # single grouping var
  DF <- clipids %>% group_by(Diet) %>% mutate(first_rep=dplyr::first(Replicate))
  expect_equal(colnames(DF), c("group", "Diet", "BileAcid", "Replicate", "first_rep"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(clipids, DF, c("group", "Diet", "BileAcid"))
  expect_equal(DF$first_rep, rep(seq.int(1, n_rows, n_rows / 2), each=n_rows / 2))


  # multiple grouping var
  DF <- clipids %>% group_by(Diet, BileAcid) %>% mutate(first_rep=dplyr::first(Replicate))
  expect_equal(colnames(DF), c("group", "Diet", "BileAcid", "Replicate", "first_rep"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(clipids, DF, c("group", "Diet", "BileAcid"))
  expect_equal(DF$first_rep, rep(seq.int(1, n_rows, n_rows / 4), each=n_rows / 4))
})

