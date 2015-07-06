calc_evi_BI <- function(x) {
   mean(sapply(seq_along(x[[2]]), function(y) x[[2]][[y]]$value)) -
     x[[1]]$value
}
