evsi_BI <- function(sens_BI, manage) {

  init <- c(1/3, 1/3)

  ui <- rbind(c(1, 0), c(0, 1), c(-1, 0), c(0, -1), c(-1, -1))
  ci <- c(0, 0, -1, -1, -1)

  e_max <- constrOptim(init, 
    function(x) {
      obj_fun_simp(x,
      BI_output=BI_output, upd.manage='NONE')},
    NULL, ui, ci, control=list(fnscale=-1))

  max_k <- mclapply(1:10, function(k) {
      constrOptim(init, 
        function(x) {
          obj_fun_simp(x, sens_BI=sens_BI, 
            BI_output=BI_output, upd.manage=manage, k)},
        NULL, ui, ci, control=list(fnscale=-1))
      }, mc.cores=detectCores())
 
  return(list(e_max, max_k))
  
}
