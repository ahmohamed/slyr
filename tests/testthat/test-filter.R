.DF <- DataFrame(
  A=rep(letters[1:2], each=6),
  B=rep(letters[1:3], each=4),
  C=1:12
)

rownames(.DF) = letters[1:12]

test_that("filter works on DFrame", {
  DF <- .DF %>% filter(C>4)
  expect_equal(DF, .DF[5:12, ])

  DF <- .DF %>% filter(C>4, C<10)
  expect_equal(DF, .DF[5:9, ])

  DF <- .DF %>% filter(A %in% "b", C<10)
  expect_equal(DF, .DF[7:9, ])
})

test_that("filter works on grouped DFrame (single grouping var)", {
  GDF <- group_by(.DF, A)
  DF <- GDF %>% filter(C>4)
  expect_equal(DF, GDF[5:12, ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:2,3:8))

  DF <- GDF %>% filter(C>4, C<10)
  expect_equal(DF, GDF[5:9, ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:2,3:5))

  DF <- GDF %>% filter(A %in% "b", C<10)
  expect_equal(DF, GDF[7:9, ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:3))
})

