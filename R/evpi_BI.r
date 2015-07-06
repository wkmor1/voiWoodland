evpi_BI <- function(BI_output, n, max_et=.2) {

  init <- c((1 - max_et) / 2, max_et / 2)

  ui <- rbind(c(1, 0), c(0, 1), c(-1, 0), c(0, -1), c(-1, -1))
  ci <- c(0, 0, -1, -max_et, -1)

  e_max <- constrOptim(init,
    function(x) {
      obj_fun_simp(x, BI_output=BI_output)},
    NULL, ui, ci, control=list(fnscale=-1))

  max_k <- lapply(sample(1:375, n), function(k) {
      constrOptim(init,
        function(x) {
          obj_fun_simp(x,
            BI_output=rapply(BI_output, function(y) y[k, ], how='replace'))},
        NULL, ui, ci, control=list(fnscale=-1))
      })

  return(list(e_max, max_k))

}
