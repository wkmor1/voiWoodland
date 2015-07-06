evsi_BI_parks <- function(BIW=NULL, BIEco=NULL, BI_output) {

  init <- .9

  e_max <- optim(init,
    function(x) {
      obj_fun_parks(x, BIW=NULL, BIEco=NULL, BI_output=BI_output)},
    NULL, lower=.8, upper=1, control=list(fnscale=-1), method="Brent")

  max_k <- lapply(seq_len(max(length(BIW), length(BIEco))),
    function(k) {
      optim(init,
        function(x) {
          obj_fun_parks(x,
            BI_output=BI_output,
            BIW=BIW, BIEco=BIEco, k)},
        NULL,  lower=.8, upper=1, control=list(fnscale=-1), method="Brent")
      })

  return(list(e_max, max_k))

}
