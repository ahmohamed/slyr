.rowdata <- DataFrame(
  A=rep(letters[1:2], each=6),
  B=rep(letters[1:3], each=4),
  C=1:12
)
rownames(.rowdata) = letters[1:12]

.coldata <- DataFrame(
  D=rep(letters[1:2], each=6),
  E=rep(letters[1:3], each=4),
  F=1:12
)
rownames(.coldata) = month.abb[1:12]

.a1 <- matrix(1:144, nrow = 12)
.a2 <- matrix(144:1, nrow = 12)

.se <- SummarizedExperiment::SummarizedExperiment(
  assays = list(A1=.a1, A2=.a2),
  rowData = .rowdata,
  colData = .coldata
)


# nrows <- 200; ncols <- 6
# counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
# rowRanges <- GenomicRanges::GRanges(rep(c("chr1", "chr2"), c(50, 150)),
#   IRanges::IRanges(floor(runif(200, 1e5, 1e6)), width=100),
#   strand=sample(c("+", "-"), 200, TRUE),
#   feature_id=sprintf("ID%03d", 1:200))
# colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#   row.names=LETTERS[1:6])
# rse <- SummarizedExperiment::SummarizedExperiment(assays=S4Vectors::SimpleList(counts=counts),
#   rowRanges=rowRanges, colData=colData)
.cols <- function(x) colnames(colData(x))
.rows <- function(x) colnames(rowData(x))

test_that("selects columns from DFrame (nameless)", {
  SE <- .se %>% select(A, B)
  expect_equal(.rows(SE), LETTERS[1:2])
  expect_equal(.cols(SE), character())
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12)) # selections don't modify dims

  #single column
  SE <- .se %>% select(D)
  expect_equal(.rows(SE), character())
  expect_equal(.cols(SE), c("D"))
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12))

  #no columns
  SE <- .se %>% select()
  expect_equal(.rows(SE), character())
  expect_equal(.cols(SE), character())
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12))

  #Different order
  SE <- .se %>% select(C, B, A)
  expect_equal(.rows(SE), c("C", "B", "A"))
  expect_equal(.cols(SE), character())
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12))

  #ranges
  SE <- .se %>% select(C:A)
  expect_equal(.rows(SE), c("C", "B", "A"))
  expect_equal(.cols(SE), character())
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12))

  # select from multiple dims
  SE <- .se %>% select(A, D, A1)
  expect_equal(.rows(SE), c("A"))
  expect_equal(.cols(SE), c("D"))
  expect_equal(assayNames(SE), c("A1"))
  expect_equal(dim(SE), c(12, 12))

  # ranged selections from multiple dims
  SE <- .se %>% select(A:A1)
  expect_equal(.rows(SE), c("A", "B", "C"))
  expect_equal(.cols(SE), c("D", "E", "F"))
  expect_equal(assayNames(SE), c("A1"))
  expect_equal(dim(SE), c(12, 12))
})


test_that("selects columns from DFrame (named)", {
  SE <- .se %>% select(X=A, Y=B)
  expect_equal(.rows(SE), c("X", "Y"))
  expect_equal(.cols(SE), character())
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12)) # selections don't modify dims
  expect_cols_equal(rowData(SE), rowData(.se), c(X="A", Y="B"))

  #single column
  SE <- .se %>% select(Z=D)
  expect_equal(.rows(SE), character())
  expect_equal(.cols(SE), c("Z"))
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12))
  expect_cols_equal(colData(SE), colData(.se), c(Z="D"))

  #Different order
  SE <- .se %>% select(Z=C, Y=B, X=A)
  expect_equal(.rows(SE), c("Z", "Y", "X"))
  expect_equal(.cols(SE), character())
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12))
  expect_cols_equal(rowData(SE), rowData(.se), c(X="A", Y="B", Z="C"))

  # select from multiple dims
  SE <- .se %>% select(X=A, Y=D, Z=A1)
  expect_equal(.rows(SE), c("X"))
  expect_equal(.cols(SE), c("Y"))
  expect_equal(assayNames(SE), c("Z"))
  expect_equal(dim(SE), c(12, 12))
  expect_cols_equal(rowData(SE), rowData(.se), c(X="A"))
  expect_cols_equal(colData(SE), colData(.se), c(Y="D"))
  expect_equal(assay(SE, "Z"), assay(.se, "A1"))
})

test_that("preserves grouping columns when selecting from grouped DFrame (single group_col)", {
  SE <- expect_message(
    .se %>% group_by(A, .dim="row") %>% select(B:A1),
    "Adding missing grouping variables: `A`"
  )
  expect_equal(.rows(SE), c("A", "B", "C"))
  expect_equal(.cols(SE), c("D", "E", "F"))
  expect_equal(assayNames(SE), c("A1"))
  expect_equal(dim(SE), c(12, 12))
  expect_equal(rowData(SE), rowData(.se) %>% group_by(A))

  #single column
  SE <- expect_message(
    .se %>% group_by(D, .dim="col") %>% select(E),
    "Adding missing grouping variables: `D`"
  )
  expect_equal(.rows(SE), character())
  expect_equal(.cols(SE), c("D", "E"))
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12))
  expect_equal(colData(SE), colData(.se) %>% group_by(D) %>% select(-`F`))

  #no columns
  SE <- expect_message(
    .se %>% group_by(D, .dim="col") %>% select(),
    "Adding missing grouping variables: `D`"
  )
  expect_equal(.rows(SE), character())
  expect_equal(.cols(SE), c("D"))
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12))
  expect_equal(colData(SE), colData(.se) %>% group_by(D) %>% select(-E, -`F`))

  #Different order

  #ranges (Already tested above)
})

test_that("preserves grouping columns when selecting from grouped DFrame (multiple group_col)", {
  SE <- expect_message(
    .se %>% group_by(A, B, .dim="row") %>% select(C:A1),
    "Adding missing grouping variables: `A`, `B`"
  )
  expect_equal(.rows(SE), c("A", "B", "C"))
  expect_equal(.cols(SE), c("D", "E", "F"))
  expect_equal(assayNames(SE), c("A1"))
  expect_equal(dim(SE), c(12, 12))
  expect_equal(rowData(SE), rowData(.se) %>% group_by(A, B))

  #single column
  SE <- expect_message(
    .se %>% group_by(D, E, .dim="col") %>% select(`F`),
    "Adding missing grouping variables: `D`, `E`"
  )
  expect_equal(.rows(SE), character())
  expect_equal(.cols(SE), c("D", "E", "F"))
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12))
  expect_equal(colData(SE), colData(.se) %>% group_by(D, E))

  #no columns
  SE <- expect_message(
    .se %>% group_by(D, E, .dim="col") %>% select(),
    "Adding missing grouping variables: `D`, `E`"
  )
  expect_equal(.rows(SE), character())
  expect_equal(.cols(SE), c("D", "E"))
  expect_equal(assayNames(SE), NULL)
  expect_equal(dim(SE), c(12, 12))
  expect_equal(colData(SE), colData(.se) %>% group_by(D, E) %>% select(-`F`))

  #Different order

  #ranges (Already tested above)
})

test_that("preserves grouping columns when selecting from grouped DFrame (multiple dims)", {
  m <- capture_messages(
    SE <- .se %>% group_by(A, .dim="row") %>% group_by(D, .dim="col") %>% select(C, `F`, A1)
  )
  expect_true(all(grepl("Adding missing grouping variables: `[AD]`", m)))
  expect_length(m, 2)
  expect_equal(.rows(SE), c("A", "C"))
  expect_equal(.cols(SE), c("D", "F"))
  expect_equal(assayNames(SE), c("A1"))
  expect_equal(dim(SE), c(12, 12))
  expect_equal(rowData(SE), rowData(.se) %>% group_by(A) %>% select(-B))
  expect_equal(colData(SE), colData(.se) %>% group_by(D) %>% select(-E))
})

test_that("dim-specific select does not affect other dims", {
  #single column
  SE <- .se %>% select(X=A, Y=B, .dim="row")
  expect_equal(.rows(SE), c("X", "Y"))
  expect_equal(colData(SE), colData(.se))
  expect_equal(assays(SE), assays(.se))
  expect_equal(dim(SE), c(12, 12)) # selections don't modify dims
  expect_cols_equal(rowData(SE), rowData(.se), c(X="A", Y="B"))

  SE <- .se %>% select(X=D, Y=E, .dim="col")
  expect_equal(.cols(SE), c("X", "Y"))
  expect_equal(rowData(SE), rowData(.se))
  expect_equal(assays(SE), assays(.se))
  expect_equal(dim(SE), c(12, 12)) # selections don't modify dims
  expect_cols_equal(colData(SE), colData(.se), c(X="D", Y="E"))

  SE <- .se %>% select(X=A1, .dim="assay")
  expect_equal(rowData(SE), rowData(.se))
  expect_equal(colData(SE), colData(.se))
  expect_equal(assayNames(SE), c("X"))
  expect_equal(dim(SE), c(12, 12)) # selections don't modify dims
  expect_equal(assay(SE, "X"), assay(.se, "A1"))

  # Grouped
  SE <- expect_silent(
    .se %>% group_by(A, B, .dim="row") %>% select(D, Y=E, .dim="col")
  )
  expect_equal(rowData(SE), rowData(.se) %>% group_by(A, B))
  expect_equal(.cols(SE), c("D", "Y"))
  expect_equal(assays(SE), assays(.se))
  expect_equal(dim(SE), c(12, 12))
  expect_cols_equal(colData(SE), colData(.se), c(D="D", Y="E"))

  m <- capture_messages(
    SE <- .se %>% group_by(A, .dim="row") %>% group_by(D, .dim="col") %>% select(X=C, B, .dim="row")
  )
  expect_true(all(grepl("Adding missing grouping variables: `A`", m)))
  expect_length(m, 1)
  expect_equal(.rows(SE), c("A", "X", "B"))
  expect_equal(colData(SE), colData(.se) %>% group_by(D))
  expect_equal(assays(SE), assays(.se))
  expect_equal(dim(SE), c(12, 12)) # selections don't modify dims
  expect_cols_equal(rowData(SE), rowData(.se), c(A="A", X="C", B="B"))

  SE <- expect_silent(
    .se %>% group_by(A, B, .dim="row") %>% group_by(D, .dim="col") %>% select(X=A1, .dim="assay")
  )
  expect_equal(rowData(SE), rowData(.se) %>% group_by(A, B))
  expect_equal(colData(SE), colData(.se) %>% group_by(D))
  expect_equal(assayNames(SE), c("X"))
  expect_equal(dim(SE), c(12, 12)) # selections don't modify dims
  expect_equal(assay(SE, "X"), assay(.se, "A1"))
})
#
# test_that("named selections renames grouping cols", {
#   gcdata <- cdata %>% group_by(cell, dex)
#
#   #single column
#   DF <- gcdata %>% select(cell2=cell, dex)
#   expect_equal(colnames(DF), c("cell2", "dex"))
#   expect_equal(rownames(DF), rownames(cdata))
#   expect_equal(colnames(group_data(DF)), c("cell2", "dex", ".rows"))
#   expect_cols_equal(group_data(DF), group_data(gcdata), c(cell2="cell", dex="dex"))
#
#   #multiple cols
#   DF <- gcdata %>% select(cell2=cell, dex2=dex)
#   expect_equal(colnames(DF), c("cell2", "dex2"))
#   expect_equal(rownames(DF), rownames(cdata))
#   expect_equal(colnames(group_data(DF)), c("cell2", "dex2", ".rows"))
#   expect_cols_equal(group_data(DF), group_data(gcdata), c(cell2="cell", dex2="dex"))
#
#   #omitting a grouping col preserves it
#   # changes the order of column in DF but not in group_data
#   DF <- expect_message(
#     gcdata %>% select(cell2=cell, Run),
#     "Adding missing grouping variables: `dex`"
#   )
#   expect_equal(colnames(DF), c("dex", "cell2", "Run"))
#   expect_equal(colnames(group_data(DF)), c("cell2", "dex", ".rows"))
#   expect_cols_equal(group_data(DF), group_data(gcdata), c(cell2="cell", dex="dex"))
# })
#
#
# test_that("rename modifies columns in DFrame", {
#   DF <- clipids %>% rename(group2=group, Diet2=Diet)
#   expect_equal(colnames(DF), c("group2", "Diet2", "BileAcid"))
#   expect_equal(rownames(DF), rownames(clipids))
#   expect_cols_equal(DF, clipids, c(group2="group", Diet2="Diet", BileAcid="BileAcid"))
#
#   #single column
#   DF <- clipids %>% rename(group2=group)
#   expect_equal(colnames(DF), c("group2", "Diet", "BileAcid"))
#   expect_equal(rownames(DF), rownames(clipids))
#   expect_cols_equal(DF, clipids, c(group2="group"))
#
#   gcdata <- clipids %>% group_by(BileAcid, Diet)
#
#   #single column
#   DF <- gcdata %>% rename(Diet2=Diet)
#   expect_equal(colnames(DF), c("group", "Diet2", "BileAcid"))
#   expect_equal(rownames(DF), rownames(clipids))
#   expect_equal(colnames(group_data(DF)), c("BileAcid", "Diet2",".rows"))
#   expect_cols_equal(group_data(DF), group_data(gcdata), c(BileAcid="BileAcid", Diet2="Diet"))
#
#   #multiple cols
#   DF <- gcdata %>% rename(Diet2=Diet, Bile2=BileAcid)
#   expect_equal(colnames(DF), c("group", "Diet2", "Bile2"))
#   expect_equal(rownames(DF), rownames(clipids))
#   expect_equal(colnames(group_data(DF)), c("Bile2", "Diet2",".rows"))
#   expect_cols_equal(group_data(DF), group_data(gcdata), c(Bile2="BileAcid", Diet2="Diet"))
# })
#
# Not needed?
# test_that("selects columns from DFrame (integer idx)", {
#   DF <- clipids %>% select(1, 2)
#   expect_equal(colnames(DF), c("group", "Diet"))
#   expect_equal(rownames(DF), rownames(clipids))
#   expect_equal(DF, clipids[, c("group", "Diet")])
#
#   #single column
#   DF <- clipids %>% select(1)
#   expect_equal(colnames(DF), c("group"))
#   expect_equal(rownames(DF), rownames(clipids))
#   expect_equal(DF, clipids[, "group", drop=FALSE])
#
#   #no columns
#   DF <- clipids %>% select(0)
#   expect_equal(colnames(DF), character())
#   expect_equal(rownames(DF), rownames(clipids))
#
#   #Different order
#   DF <- clipids %>% select(2, 1)
#   expect_equal(colnames(DF), c("Diet", "group"))
#   expect_equal(rownames(DF), rownames(clipids))
#   expect_equal(DF, clipids[, c("Diet", "group")])
#
#   #ranges
#   DF <- clipids %>% select(3:1)
#   expect_equal(colnames(DF), c("BileAcid", "Diet", "group"))
#   expect_equal(rownames(DF), rownames(clipids))
#   expect_equal(DF, clipids[, c("BileAcid", "Diet", "group")])
# })
#

