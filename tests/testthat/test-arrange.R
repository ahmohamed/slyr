.DF <- DataFrame(
  A=rep(letters[1:2], each=6),
  B=rep(letters[1:3], each=4),
  C=1:12
)

rownames(.DF) = letters[1:12]

test_that("arrange works on DFrame", {
  DF <- .DF %>% arrange(-C)
  expect_equal(DF, .DF[12:1, ])

  DF <- .DF %>% arrange(A, -C)
  expect_equal(DF, .DF[c(6:1, 12:7), ])
})

test_that("arrange works on grouped DFrame (single grouping var)", {
  GDF <- group_by(.DF, A)
  DF <- GDF %>% arrange(-C)
  expect_equal(DF, GDF[12:1, ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(7:12, 1:6))

  DF <- GDF %>% arrange(-C, .by_group = TRUE)
  expect_equal(DF, GDF[c(6:1, 12:7), ] %>% copy_groups(DF))
  expect_equal(as.list(group_data(DF)$.rows), list(1:6, 7:12))
})

