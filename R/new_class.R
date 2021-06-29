group_data.se_list <- dplyr:::group_data.data.frame
group_keys.se_list <- dplyr:::group_keys.data.frame
group_vars.se_list <- dplyr:::group_vars.data.frame
dim.se_list <- function(x) {
  main_dim <- attr(x, "main_dim")
  .nrow <- length(x[(attr(x, "idx") == main_dim)][[1]])
  c(.nrow, length(x))
}

get_se_list <- function(se, main_dim="row") {
  se_list <- se_data_mask(se)
  attr(se_list, "main_dim") <- main_dim
  attr(se_list, "idx") <- se_data_idx(se)
  class(se_list) <- c("se_list", class(se_list))
  se_list
}

`[.se_list` <- function(x, i, ...) {
  attrs <- attributes(x)
  out <- unclass(x)
  out <- out[i]
  attr(out, "main_dim") <- attr(x, "main_dim")
  attr(out, "idx") <- attr(x, "idx")
  class(out) <- c(class(x), "data.frame") # data.frame class needed for dplyr_col_select
  out
}

# dplyr:::mutate_cols(get_se_list(.se), X=C+`F`)
# .arrange_loc(get_se_list(.se), desc(`F`))

# .rowdata <- DataFrame(
#   A=rep(letters[1:3], each=6),
#   B=rep(letters[1:6], each=3),
#   C=1:18
# )
#
# rownames(.rowdata) = letters[1:18]
#
# .coldata <- DataFrame(
#   D=rep(letters[1:2], each=6),
#   E=rep(letters[1:3], each=4),
#   F=1:12
# )
# rownames(.coldata) = month.abb[1:12]
#
# .a1 <- matrix(1:216, nrow = 18)
# .a2 <- matrix(216:1, nrow = 18)
# .se <- SummarizedExperiment::SummarizedExperiment(
#   assays = list(A1=.a1, A2=.a2),
#   rowData = .rowdata,
#   colData = .coldata
# )
