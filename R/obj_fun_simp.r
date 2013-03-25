obj_fun_simp <- function(x, BI_output, BIW=NULL, BIMan=NULL, BIEco=NULL, k) {
  
  A <- x[1]
  B <- x[2]
  
  if (is.null(BIW)) {
    BIW <- unlist(BI_output[['BIW']][['MAT']][[15]])
  } else {
    BIW <- unlist(BIW[[k]][['MAT']][[15]])
  }
  
  if (is.null(BIMan)) {
    BIMan <- unlist(BI_output[['BIMan']][['MAT']][[15]])
  } else {
    BIMan <- unlist(BIMan[[k]][['MAT']][[15]])
  }
  
  if (is.null(BIEco)) {
    BIEco <- unlist(BI_output[['BIEco']][['MAT']][[15]])
  } else {
    BIEco <- unlist(BIEco[[k]][['MAT']][[15]])
  }
     
  mean(A * BIW + B * BIEco + (1 - A - B) * BIMan) 
  
}
