sse_norm <- function(may_df){
  sse <- sum((may_df$obs - may_df$model)^2)
  sse_norm <- 1 - sse
  return(sse_norm)
}