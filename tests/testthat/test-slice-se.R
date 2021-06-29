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


test_that("slice works on SE (rows)", {
  SE <- .se %>% slice(1, .dim="row")
  expect_equal(SE, .se[1, ])

  SE <- .se %>% slice(1, 2, 3, .dim="row")
  expect_equal(SE, .se[1:3, ])

  SE <- .se %>% slice(which(C > 15), .dim="row")
  expect_equal(SE, .se[16:18, ])

  SE <- .se %>% slice_head(n=3, .dim="row")
  expect_equal(SE, .se[1:3, ])

  SE <- .se %>% slice_tail(n=3, .dim="row")
  expect_equal(SE, .se[16:18, ])

  SE <- .se %>% slice_max(C, n = 3, .dim="row")
  expect_equal(SE, .se[18:16, ])

  SE <- .se %>% slice_min(C, n = 3, .dim="row")
  expect_equal(SE, .se[1:3, ])

  set.seed(1)
  SE <- .se %>% slice_sample(n=3, .dim="row")
  expect_equal(SE, .se[c(4, 7, 1), ])
})

test_that("slice works on SE (cols)", {
  SE <- .se %>% slice(1, .dim="col")
  expect_equal(SE, .se[, 1])

  SE <- .se %>% slice(1, 2, 3, .dim="col")
  expect_equal(SE, .se[, 1:3])

  SE <- .se %>% slice(which(`F` > 9), .dim="col")
  expect_equal(SE, .se[, 10:12])

  SE <- .se %>% slice_head(n=3, .dim="col")
  expect_equal(SE, .se[, 1:3])

  SE <- .se %>% slice_tail(n=3, .dim="col")
  expect_equal(SE, .se[, 10:12])

  SE <- .se %>% slice_max(`F`, n = 3, .dim="col")
  expect_equal(SE, .se[, 12:10])

  SE <- .se %>% slice_min(`F`, n = 3, .dim="col")
  expect_equal(SE, .se[, 1:3])

  set.seed(1)
  SE <- .se %>% slice_sample(n=3, .dim="col")
  expect_equal(SE, .se[, c(9, 4, 7)])
})


test_that("slice works on SE with data masking", {
  SE <- .se %>% slice(which(rowSums(A1) > 1300), .dim="row")
  expect_equal(SE, .se[10:18,])

  SE <- .se %>% slice_max(colSums(A1), n = 3, .dim="col")
  expect_equal(SE, .se[, 12:10])

  SE <- .se %>% slice_min(colSums(A1), n = 3, .dim="col")
  expect_equal(SE, .se[, 1:3])

  # multiple dims
  SE <- .se %>% slice(which(rowSums(A1[, `F` >5]) > 1071), .dim="row")
  expect_equal(SE, .se[10:18,])

  SE <- .se %>% slice_max(colSums(A1[C>5, ]), n = 3, .dim="col")
  expect_equal(SE, .se[, 12:10])

  SE <- .se %>% slice_min(colSums(A1[C>5, ]), n = 3, .dim="col")
  expect_equal(SE, .se[, 1:3])
})

test_that("slice works on grouped SE (rows)", {
  .se <- .se %>% group_by(A, .dim="row") %>% group_by(D, .dim="col")
  SE <- .se %>% slice(1, .dim="row")
  expect_equal(ungroup(SE), ungroup(.se[c(1, 7, 13), ]))
  expect_equal(rowData(SE), slice(rowData(.se), 1))

  SE <- .se %>% slice(1, 2, 3, .dim="row")
  expect_equal(ungroup(SE), ungroup(.se[c(1:3, 7:9, 13:15), ]))
  expect_equal(rowData(SE), slice(rowData(.se), 1, 2, 3))

  #external var
  V <- 1:3
  SE <- .se %>% slice(V, .dim="row")
  expect_equal(ungroup(SE), ungroup(.se[c(1:3, 7:9, 13:15), ]))
  expect_equal(rowData(SE), slice(rowData(.se), V))

  SE <- .se %>% slice_head(n=3, .dim="row")
  expect_equal(ungroup(SE), ungroup(.se[c(1:3, 7:9, 13:15), ]))
  expect_equal(rowData(SE), slice_head(rowData(.se), n=3))

  SE <- .se %>% slice_tail(n=3, .dim="row")
  expect_equal(ungroup(SE), ungroup(.se[c(4:6, 10:12, 16:18), ]))
  expect_equal(rowData(SE), slice_tail(rowData(.se), n=3))

  SE <- .se %>% slice_max(C, n = 3, .dim="row")
  expect_equal(ungroup(SE), ungroup(.se[c(6:4, 12:10, 18:16), ]))
  expect_equal(rowData(SE), slice_max(rowData(.se), C, n=3))

  SE <- .se %>% slice_min(C, n = 3, .dim="row")
  expect_equal(ungroup(SE), ungroup(.se[c(1:3, 7:9, 13:15), ]))
  expect_equal(rowData(SE), slice_min(rowData(.se), C, n=3))

  set.seed(1)
  SE <- .se %>% slice_sample(n=2, .dim="row")
  expect_equal(ungroup(SE), ungroup(.se[c(1,4,7,8,17,15), ]))
  expect_equal(rowData(SE), slice(rowData(.se), 1,4,7,8,17,15))
})

# test_that("slice works on SE (cols)", {
#   SE <- .se %>% slice(1, .dim="col")
#   expect_equal(SE, .se[, 1])
#
#   SE <- .se %>% slice(1, 2, 3, .dim="col")
#   expect_equal(SE, .se[, 1:3])
#
#   SE <- .se %>% slice(which(`F` > 9), .dim="col")
#   expect_equal(SE, .se[, 10:12])
#
#   SE <- .se %>% slice_head(n=3, .dim="col")
#   expect_equal(SE, .se[, 1:3])
#
#   SE <- .se %>% slice_tail(n=3, .dim="col")
#   expect_equal(SE, .se[, 10:12])
#
#   SE <- .se %>% slice_max(`F`, n = 3, .dim="col")
#   expect_equal(SE, .se[, 12:10])
#
#   SE <- .se %>% slice_min(`F`, n = 3, .dim="col")
#   expect_equal(SE, .se[, 1:3])
#
#   set.seed(1)
#   SE <- .se %>% slice_sample(n=3, .dim="col")
#   expect_equal(SE, .se[, c(9, 4, 7)])
# })
#
#
# test_that("slice works on SE with data masking", {
#   SE <- .se %>% slice(which(rowSums(A1) > 1300), .dim="row")
#   expect_equal(SE, .se[10:18,])
#
#   SE <- .se %>% slice_max(colSums(A1), n = 3, .dim="col")
#   expect_equal(SE, .se[, 12:10])
#
#   SE <- .se %>% slice_min(colSums(A1), n = 3, .dim="col")
#   expect_equal(SE, .se[, 1:3])
#
#   # multiple dims
#   SE <- .se %>% slice(which(rowSums(A1[, `F` >5]) > 1071), .dim="row")
#   expect_equal(SE, .se[10:18,])
#
#   SE <- .se %>% slice_max(colSums(A1[C>5, ]), n = 3, .dim="col")
#   expect_equal(SE, .se[, 12:10])
#
#   SE <- .se %>% slice_min(colSums(A1[C>5, ]), n = 3, .dim="col")
#   expect_equal(SE, .se[, 1:3])
# })
