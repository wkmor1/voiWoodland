obj_fun_simp <- function(x, sens_BI, BI_output, upd.manage=c('BIW', 'BIMan', 'BIEco', 'NONE'), k) {
  A <- x[1]
  B <- x[2]
  
  upd.manage <- match.arg(upd.manage)
  
  BIW <- switch(upd.manage,
    BIW = unlist(sens_BI[[k]][['MAT']][[15]]),
    BIMan = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 'BIW' & SSAbbr == 'MAT', select=Area)),
    BIEco = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 'BIW' & SSAbbr == 'MAT', select=Area)),
    NONE = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 'BIW' & SSAbbr == 'MAT', select=Area)))
  
  BIMan <- switch(upd.manage,
    BIMan = unlist(sens_BI[[k]][['MAT']][[15]]),
    BIW = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 'BIMan' & SSAbbr == 'MAT', select=Area)),
    BIEco = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 'BIMan' & SSAbbr == 'MAT', select=Area)),
    NONE = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 'BIMan' & SSAbbr == 'MAT', select=Area)))
  
  BIEco <- switch(upd.manage,
    BIEco = unlist(sens_BI[[k]][['MAT']][[15]]),
    BIW = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 'BIEco' & SSAbbr == 'MAT', select=Area)),
    BIMan = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 'BIEco' & SSAbbr == 'MAT', select=Area)),
    NONE = unlist(subset(BI_output, Timestep == 150 & CTAbbr == 'BIEco' & SSAbbr == 'MAT', select=Area)))
  
  mean(A * BIW + B * BIMan + (1 - A - B) * BIEco) 
  
}
