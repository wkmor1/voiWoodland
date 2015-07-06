evpi_BI_parks <- function(BI_output, n) {

  init <- .9

  e_max <- optim(init,
    function(x) {
      obj_fun_parks(x, BI_output=BI_output)},
    NULL, lower=.8, upper=1, control=list(fnscale=-1), method="Brent")

  max_k <- lapply(sample(1:375, n), function(k) {
      optim(init,
        function(x) {
          obj_fun_parks(x,
            BI_output=rapply(BI_output, function(y) y[k, ], how='replace'))},
        NULL, lower=.8, upper=1, control=list(fnscale=-1), method="Brent")
      })

  return(list(e_max, max_k))

}
