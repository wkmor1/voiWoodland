evsi_BI <- function(BIW=NULL, BIMan=NULL, BIEco=NULL, BI_output) {
  
  init <- c(1/3, .1)

  ui <- rbind(c(1, 0), c(0, 1), c(-1, 0), c(0, -1), c(-1, -1))
  ci <- c(0, 0, -1, -.2, -1)

  e_max <- constrOptim(init, 
    function(x) {
      obj_fun_simp(x, BIW=NULL, BIMan=NULL, BIEco=NULL, BI_output=BI_output)},
    NULL, ui, ci, control=list(fnscale=-1))

  max_k <- mclapply(seq_len(max(length(BIW), length(BIMan), length(BIEco))), 
    function(k) {
      constrOptim(init, 
        function(x) {
          obj_fun_simp(x,
            BI_output=BI_output,
            BIW=BIW, BIMan=BIMan, BIEco=BIEco, k)},
        NULL, ui, ci, control=list(fnscale=-1))
      }, mc.cores=5)
 
  return(list(e_max, max_k))
  
}
