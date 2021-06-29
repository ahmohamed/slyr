.rowdata <- DataFrame(
  A=rep(letters[1:3], each=6),
  B=rep(letters[1:6], each=3),
  C=1:18
)

rownames(.rowdata) = letters[1:18]

.coldata <- DataFrame(
  D=rep(letters[1:2], each=6),
  E=rep(letters[1:3], each=4),
  F=1:12
)
rownames(.coldata) = month.abb[1:12]

.a1 <- matrix(1:216, nrow = 18)
.a2 <- matrix(216:1, nrow = 18)
.se <- SummarizedExperiment::SummarizedExperiment(
  assays = list(A1=.a1, A2=.a2),
  rowData = .rowdata,
  colData = .coldata
)

test_that("filter works on SE", {
  SE <- .se %>% filter(C>4, .dim="row")
  expect_equal(SE, .se[5:18, ])

  SE <- .se %>% filter(`F`>4, `F`<10, .dim="col")
  expect_equal(SE, .se[, 5:9])

  SE <- .se %>% filter(D %in% "b", `F`<10, .dim="col")
  expect_equal(SE, .se[, 7:9])

  # Reference other dims
  SE <- .se %>% filter(rowSums(A1) > 1300, .dim="row")
  expect_equal(SE, .se[10:18, ])

  SE <- .se %>% filter(colSums(A1) > 2000, .dim="col")
  expect_equal(SE, .se[, 7:12])

  # Reference multiple dims
  SE <- .se %>% filter(rowSums(A1[, `F` > 5]) > 1071, .dim="row")
  expect_equal(SE, .se[10:18, ])

  SE <- .se %>% filter(colSums(A1[C > 5, ]) > 1400, .dim="col")
  expect_equal(SE, .se[, 7:12])
})

# test_that("filter works on grouped SErame (single grouping var)", {
#   GSE <- group_by(.se, A)
#   SE <- GSE %>% filter(C>4)
#   expect_equal(SE, GSE[5:12, ] %>% copy_groups(SE))
#   expect_equal(as.list(group_data(SE)$.rows), list(1:2,3:8))
#
#   SE <- GSE %>% filter(C>4, C<10)
#   expect_equal(SE, GSE[5:9, ] %>% copy_groups(SE))
#   expect_equal(as.list(group_data(SE)$.rows), list(1:2,3:5))
#
#   SE <- GSE %>% filter(A %in% "b", C<10)
#   expect_equal(SE, GSE[7:9, ] %>% copy_groups(SE))
#   expect_equal(as.list(group_data(SE)$.rows), list(1:3))
# })

