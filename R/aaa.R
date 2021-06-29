forDFrame <- function(fn) {
  function(DF, ...)
    DF %>% to_df() %>% fn(...) %>% to_DFrame()
}
