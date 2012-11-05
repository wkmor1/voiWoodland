calc_evsi_BI <- function(x) {
   mean(sapply(1:10, function(y) x[[2]][[y]]$value)) -
     x[[1]]$value
 }