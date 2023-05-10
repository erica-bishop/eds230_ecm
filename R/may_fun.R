spring_range <- function(maymax_o, maymax_m, maymin_o, maymin_m){
  range_o <- maymax_o - maymin_o
  range_m <- maymax_m - maymin_m
  
  cor_may <- cor(range_o, range_m)
  
  return(cor_may)
}
