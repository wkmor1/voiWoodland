obj_fun_parks <- function(x, BI_output, BIW=NULL, BIEco=NULL, k) {
  
  A <- x
  
  if (is.null(BIW)) {
    BIW <- unlist(BI_output[['BIW']][['MAT']][[15]])
  } else {
    BIW <- unlist(BIW[[k]][['MAT']][[15]])
  }
  
  if (is.null(BIEco)) {
    BIEco <- unlist(BI_output[['BIEco']][['MAT']][[15]])
  } else {
    BIEco <- unlist(BIEco[[k]][['MAT']][[15]])
  }
     
  mean(A * BIW + (1 - A) * BIEco) 
  
}
