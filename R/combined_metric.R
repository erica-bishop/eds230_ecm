ecm <- function(mod, obs){
  
  #set up max and min inputs
  max_o <- max(obs)
  
  max_m <- max(mod)
  
  min_o <- min(obs)
  
  min_m <- min(mod)
  
  #correlation
  range_o <- max_o - min_o
  range_m <- max_m - min_m
  
  cor <- cor(range_o, range_m)
  
  #sse 
    sse <- sum((obs - mod)^2)
    sse_norm <- 1 - sse
  
  #combined metric
  ecm_metric <- sqrt(cor*sse)
  
  return(ecm_metric)
}