.DF <- DataFrame(
  A=rep(letters[1:2], each=6),
  B=rep(letters[1:3], each=4),
  C=1:12
)

rownames(.DF) = letters[1:12]

test_that("slice works on DFrame", {
  DF <- .DF %>% slice(1)
  expect_equal(DF, .DF[1, ])

  DF <- .DF %>% slice(1, 2, 3)
  expect_equal(DF, .DF[1:3, ])

  DF <- .DF %>% slice_head(n=3)
  expect_equal(DF, .DF[1:3, ])

  DF <- .DF %>% slice_tail(n=3)
  expect_equal(DF, .DF[10:12, ])

  DF <- .DF %>% slice_max(C, n = 3)
  expect_equal(DF, .DF[12:10, ])

  DF <- .DF %>% slice_min(C, n = 3)
  expect_equal(DF, .DF[1:3, ])

  set.seed(1)
  DF <- .DF %>% slice_sample(n=3)
  expect_equal(DF, .DF[c(9, 4, 7), ])
})

test_that("slice works on grouped DFrame (single grouping var)", {
  GDF <- group_by(.DF, A)
  DF <- GDF %>% slice(1)
  expect_equal(DF, GDF[c(1,7), ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1,2))

  DF <- GDF %>% slice(1, 2, 3)
  expect_equal(DF, GDF[c(1:3, 7:9), ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:3,4:6))

  #external var
  V <- 1:3
  DF <- GDF %>% slice(V)
  expect_equal(DF, GDF[c(1:3, 7:9), ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:3,4:6))

  DF <- GDF %>% slice_head(n=3)
  expect_equal(DF, GDF[c(1:3, 7:9), ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:3,4:6))

  DF <- GDF %>% slice_tail(n=3)
  expect_equal(DF, GDF[c(4:6, 10:12), ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:3,4:6))

  DF <- GDF %>% slice_max(C, n = 2)
  expect_equal(DF, GDF[c(6:5, 12:11), ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:2, 3:4))

  DF <- GDF %>% slice_min(C, n = 2)
  expect_equal(DF, GDF[c(1:2, 7:8), ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:2, 3:4))

  V <- .DF$C
  DF <- GDF %>% slice_max(V, n = 2)
  expect_failure({ # known bug
    expect_equal(DF, GDF[c(6:5, 12:11), ] %>% copy_groups(DF))
    expect_equal(as.list(group_data(DF)$.rows), list(1:2, 3:4))
  })

  DF <- GDF %>% slice_min(V, n = 2)
  expect_equal(DF, GDF[c(1:2, 7:8), ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:2, 3:4))


  set.seed(1)
  DF <- GDF %>% slice_sample(n=3)
  expect_equal(DF, GDF[c(1, 4, 3, 7, 8, 12), ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:3,4:6))
})

"slice works on grouped DFrame (multiple grouping var)"
