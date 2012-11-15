evpi_BI <- function(BI_output, n) {

  init <- c(1/3, 1/3)

  ui <- rbind(c(1, 0), c(0, 1), c(-1, 0), c(0, -1), c(-1, -1))
  ci <- c(0, 0, -1, -1, -1)

  e_max <- constrOptim(init, 
    function(x) {
      obj_fun_simp(x,
      BI_output=BI_output, upd.manage='NONE')},
    NULL, ui, ci, control=list(fnscale=-1))

  max_k <- mclapply(levels(BI_output$Project)[sample(1:375, n)], function(k) {
      constrOptim(init, 
        function(x) {
          obj_fun_simp(x, 
            BI_output=subset(BI_output, Project == k), upd.manage='NONE')},
        NULL, ui, ci, control=list(fnscale=-1))
      }, mc.cores=5)
 
  return(list(e_max, max_k))
  
}
