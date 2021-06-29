## code to prepare `lipids` dataset goes here
library(lipidr)
data(data_normalized)
lipids <- as(data_normalized, "SummarizedExperiment")
usethis::use_data(lipids, overwrite = TRUE)
