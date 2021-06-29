prepare_data_mask <- function(se, .dim) {
  rowdata <- append_rownames(rowData(se), ".row")
  coldata <- append_rownames(colData(se), ".col")

  if (.dim == 'row') {
    list(
      DF=rowdata,
      mask=append(assays(se)@listData, coldata@listData)
    )
  } else {
    list(
      DF=coldata,
      mask=append(assays(se)@listData, rowdata@listData)
    )
  }
}

append_rownames <- function(DF, name) {
  DF@listData[[name]] <- DF@rownames
  DF
}
