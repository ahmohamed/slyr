airway <- readRDS("airway.RDS")
rdata <- rowData(airway)
rdf <- as.data.frame(rdata)

cdata <- colData(airway)
cdf <- as.data.frame(cdata)

data("lipids")
clipids <- colData(lipids)
rlipids <- rowData(lipids)

test_that("selects columns from DFrame (nameless)", {
  DF <- clipids %>% select(group, Diet)
  expect_equal(colnames(DF), c("group", "Diet"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(DF, clipids[, c("group", "Diet")])

  #single column
  DF <- clipids %>% select(group)
  expect_equal(colnames(DF), c("group"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(DF, clipids[, "group", drop=FALSE])

  #no columns
  DF <- clipids %>% select()
  expect_equal(colnames(DF), character())
  expect_equal(rownames(DF), rownames(clipids))

  #Different order
  DF <- clipids %>% select(Diet, group)
  expect_equal(colnames(DF), c("Diet", "group"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(DF, clipids[, c("Diet", "group")])

  #ranges
  DF <- clipids %>% select(BileAcid:group)
  expect_equal(colnames(DF), c("BileAcid", "Diet", "group"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(DF, clipids[, c("BileAcid", "Diet", "group")])
})


test_that("selects columns from DFrame (named)", {
  DF <- clipids %>% select(group2=group, Diet2=Diet)
  expect_equal(colnames(DF), c("group2", "Diet2"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(DF, clipids, c(group2="group", Diet2="Diet"))

  #single column
  DF <- clipids %>% select(group2=group)
  expect_equal(colnames(DF), c("group2"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(DF, clipids, c(group2="group"))

  #Different order
  DF <- clipids %>% select(b=Diet, a=group)
  expect_equal(colnames(DF), c("b", "a"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(DF, clipids, c(a="group", b="Diet"))
})

test_that("selects columns from DFrame (integer idx)", {
  DF <- clipids %>% select(1, 2)
  expect_equal(colnames(DF), c("group", "Diet"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(DF, clipids[, c("group", "Diet")])

  #single column
  DF <- clipids %>% select(1)
  expect_equal(colnames(DF), c("group"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(DF, clipids[, "group", drop=FALSE])

  #no columns
  DF <- clipids %>% select(0)
  expect_equal(colnames(DF), character())
  expect_equal(rownames(DF), rownames(clipids))

  #Different order
  DF <- clipids %>% select(2, 1)
  expect_equal(colnames(DF), c("Diet", "group"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(DF, clipids[, c("Diet", "group")])

  #ranges
  DF <- clipids %>% select(3:1)
  expect_equal(colnames(DF), c("BileAcid", "Diet", "group"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(DF, clipids[, c("BileAcid", "Diet", "group")])
})

test_that("preserves grouping columns when selecting from grouped DFrame (single group_col)", {
  DF <- expect_message(
    clipids %>% group_by(BileAcid) %>% select(group, Diet),
    "Adding missing grouping variables: `BileAcid`"
  )
  expect_equal(colnames(DF), c("BileAcid", "group", "Diet"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(DF, clipids, c("BileAcid", "group", "Diet"))

  #single column
  DF <- expect_message(
    clipids %>% group_by(BileAcid) %>% select(group),
    "Adding missing grouping variables: `BileAcid`"
  )
  expect_equal(colnames(DF), c("BileAcid", "group"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(DF, clipids, c("BileAcid", "group"))

  #no columns
  DF <- expect_message(
    clipids %>% group_by(BileAcid) %>% select(),
    "Adding missing grouping variables: `BileAcid`"
  )
  expect_equal(colnames(DF), c("BileAcid"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(DF, clipids, c("BileAcid"))

  #Different order
  DF <- expect_message(
    clipids %>% group_by(BileAcid) %>% select(Diet, group),
    "Adding missing grouping variables: `BileAcid`"
  )
  expect_equal(colnames(DF), c("BileAcid", "Diet", "group"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(DF, clipids, c("BileAcid", "Diet", "group"))

  #ranges
  DF <- expect_message(
    clipids %>% group_by(BileAcid) %>% select(Diet:group),
    "Adding missing grouping variables: `BileAcid`"
  )
  expect_equal(colnames(DF), c("BileAcid", "Diet", "group"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(DF, clipids, c("BileAcid", "Diet", "group"))
})

test_that("preserves grouping columns when selecting from grouped DFrame (multiple group_col)", {
  DF <- expect_message(
    cdata %>% group_by(cell, dex) %>% select(Run, Sample),
    "Adding missing grouping variables: `cell`, `dex`"
  )
  expect_equal(colnames(DF), c("cell", "dex", "Run", "Sample"))
  expect_equal(rownames(DF), rownames(cdata))
  expect_cols_equal(DF, cdata, c("cell", "dex", "Run", "Sample"))

  #single column
  DF <- expect_message(
    cdata %>% group_by(cell, dex) %>% select(Run),
    "Adding missing grouping variables: `cell`, `dex`"
  )
  expect_equal(colnames(DF), c("cell", "dex", "Run"))
  expect_equal(rownames(DF), rownames(cdata))
  expect_cols_equal(DF, cdata, c("cell", "dex", "Run"))

  #no columns
  DF <- expect_message(
    cdata %>% group_by(cell, dex) %>% select(),
    "Adding missing grouping variables: `cell`, `dex`"
  )
  expect_equal(colnames(DF), c("cell", "dex"))
  expect_equal(rownames(DF), rownames(cdata))
  expect_cols_equal(DF, cdata, c("cell", "dex"))

  #Different order
  DF <- expect_message(
    cdata %>% group_by(cell, dex) %>% select(Sample, Run),
    "Adding missing grouping variables: `cell`, `dex`"
  )
  expect_equal(colnames(DF), c("cell", "dex", "Sample", "Run"))
  expect_equal(rownames(DF), rownames(cdata))
  expect_cols_equal(DF, cdata, c("cell", "dex", "Run", "Sample"))

  #ranges
  DF <- expect_message(
    cdata %>% group_by(cell, dex) %>% select(Sample:Run),
    "Adding missing grouping variables: `cell`, `dex`"
  )
  expect_equal(colnames(DF), c("cell", "dex", "Sample", "Experiment", "avgLength", "Run"))
  expect_equal(rownames(DF), rownames(cdata))
  expect_cols_equal(DF, cdata, c("cell", "dex", "Sample", "Experiment", "avgLength", "Run"))
})

test_that("named selections renames grouping cols", {
  gcdata <- cdata %>% group_by(cell, dex)

  #single column
  DF <- gcdata %>% select(cell2=cell, dex)
  expect_equal(colnames(DF), c("cell2", "dex"))
  expect_equal(rownames(DF), rownames(cdata))
  expect_equal(colnames(group_data(DF)), c("cell2", "dex", ".rows"))
  expect_cols_equal(group_data(DF), group_data(gcdata), c(cell2="cell", dex="dex"))

  #multiple cols
  DF <- gcdata %>% select(cell2=cell, dex2=dex)
  expect_equal(colnames(DF), c("cell2", "dex2"))
  expect_equal(rownames(DF), rownames(cdata))
  expect_equal(colnames(group_data(DF)), c("cell2", "dex2", ".rows"))
  expect_cols_equal(group_data(DF), group_data(gcdata), c(cell2="cell", dex2="dex"))

  #omitting a grouping col preserves it
  # changes the order of column in DF but not in group_data
  DF <- expect_message(
    gcdata %>% select(cell2=cell, Run),
    "Adding missing grouping variables: `dex`"
  )
  expect_equal(colnames(DF), c("dex", "cell2", "Run"))
  expect_equal(colnames(group_data(DF)), c("cell2", "dex", ".rows"))
  expect_cols_equal(group_data(DF), group_data(gcdata), c(cell2="cell", dex="dex"))
})


test_that("rename modifies columns in DFrame", {
  DF <- clipids %>% rename(group2=group, Diet2=Diet)
  expect_equal(colnames(DF), c("group2", "Diet2", "BileAcid"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(DF, clipids, c(group2="group", Diet2="Diet", BileAcid="BileAcid"))

  #single column
  DF <- clipids %>% rename(group2=group)
  expect_equal(colnames(DF), c("group2", "Diet", "BileAcid"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_cols_equal(DF, clipids, c(group2="group"))

  gcdata <- clipids %>% group_by(BileAcid, Diet)

  #single column
  DF <- gcdata %>% rename(Diet2=Diet)
  expect_equal(colnames(DF), c("group", "Diet2", "BileAcid"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(colnames(group_data(DF)), c("BileAcid", "Diet2",".rows"))
  expect_cols_equal(group_data(DF), group_data(gcdata), c(BileAcid="BileAcid", Diet2="Diet"))

  #multiple cols
  DF <- gcdata %>% rename(Diet2=Diet, Bile2=BileAcid)
  expect_equal(colnames(DF), c("group", "Diet2", "Bile2"))
  expect_equal(rownames(DF), rownames(clipids))
  expect_equal(colnames(group_data(DF)), c("Bile2", "Diet2",".rows"))
  expect_cols_equal(group_data(DF), group_data(gcdata), c(Bile2="BileAcid", Diet2="Diet"))
})

