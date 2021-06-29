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

test_that("arrange works on SE (rows)", {
  SE <- .se %>% arrange(-C, .dim="row")
  expect_equal(SE, .se[18:1, ])

  SE <- .se %>% arrange(A, -C, .dim="row")
  expect_equal(SE, .se[c(6:1, 12:7, 18:13), ])
})

test_that("arrange works on SE (cols)", {
  SE <- .se %>% arrange(-`F`, .dim="col")
  expect_equal(SE, .se[, 12:1])

  SE <- .se %>% arrange(D, -`F`, .dim="col")
  expect_equal(SE, .se[, c(6:1, 12:7)])
})

test_that("arrange works on grouped DFrame (single grouping var) (row)", {
  GSE <- group_by(.se, A, .dim="row")

  SE <- GSE %>% arrange(-C, .dim="row")
  # remove groups to be able to compare
  row_data <- rowData(SE)
  gpdata <- group_data(row_data)
  attr(row_data, "groups") <- NULL
  rowData(SE) <- row_data
  expect_equal(SE, .se[c(18:1), ])
  expect_equal(as.list(gpdata$.rows), list(13:18, 7:12, 1:6))

  SE <- GSE %>% arrange(-C, .dim="row", .by_group = TRUE)
  # remove groups to be able to compare
  row_data <- rowData(SE)
  gpdata <- group_data(row_data)
  attr(row_data, "groups") <- NULL
  rowData(SE) <- row_data
  expect_equal(SE, .se[c(6:1, 12:7, 18:13), ])
  expect_equal(as.list(gpdata$.rows), list(1:6, 7:12, 13:18))


  # referencing other assay
  SE <- GSE %>% arrange(-rowSums(A1), .dim="row", .by_group = TRUE)
  row_data <- rowData(SE)
  gpdata <- group_data(row_data)
  attr(row_data, "groups") <- NULL
  rowData(SE) <- row_data
  expect_equal(SE, .se[c(6:1, 12:7, 18:13), ])
  expect_equal(as.list(gpdata$.rows), list(1:6, 7:12, 13:18))

  # referencing other assay and coldata
  SE <- GSE %>% arrange(-rowSums(A1[, `F` > 6]), .dim="row", .by_group = TRUE)
  row_data <- rowData(SE)
  gpdata <- group_data(row_data)
  attr(row_data, "groups") <- NULL
  rowData(SE) <- row_data
  expect_equal(SE, .se[c(6:1, 12:7, 18:13), ])
  expect_equal(as.list(gpdata$.rows), list(1:6, 7:12, 13:18))
})

test_that("arrange works on grouped DFrame (single grouping var) (col)", {
  GSE <- group_by(.se, D, .dim="col")

  SE <- GSE %>% arrange(-`F`, .dim="col")
  # remove groups to be able to compare
  col_data <- colData(SE)
  gpdata <- group_data(col_data)
  attr(col_data, "groups") <- NULL
  colData(SE) <- col_data
  expect_equal(SE, .se[, c(12:1)])
  expect_equal(as.list(gpdata$.rows), list(7:12, 1:6))

  SE <- GSE %>% arrange(-`F`, .dim="col", .by_group = TRUE)
  # remove groups to be able to compare
  col_data <- colData(SE)
  gpdata <- group_data(col_data)
  attr(col_data, "groups") <- NULL
  colData(SE) <- col_data
  expect_equal(SE, .se[, c(6:1, 12:7)])
  expect_equal(as.list(gpdata$.rows), list(1:6, 7:12))

  # referencing other assay
  SE <- GSE %>% arrange(-colSums(A1), .dim="col", .by_group = TRUE)
  # remove groups to be able to compare
  col_data <- colData(SE)
  gpdata <- group_data(col_data)
  attr(col_data, "groups") <- NULL
  colData(SE) <- col_data
  expect_equal(SE, .se[, c(6:1, 12:7)])
  expect_equal(as.list(gpdata$.rows), list(1:6, 7:12))

  # referencing other assay and rowdata
  SE <- GSE %>% arrange(-colSums(A1[C > 6, ]), .dim="col", .by_group = TRUE)
  # remove groups to be able to compare
  col_data <- colData(SE)
  gpdata <- group_data(col_data)
  attr(col_data, "groups") <- NULL
  colData(SE) <- col_data
  expect_equal(SE, .se[, c(6:1, 12:7)])
  expect_equal(as.list(gpdata$.rows), list(1:6, 7:12))
})
