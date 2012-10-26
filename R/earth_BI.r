earth_BI <- function(n_boot, BIModelOutput, inputs, dir, verbose=FALSE) {
    
  for (i in names(inputs)) {
      
    for (j in c('LDW', 'HDW', 'MAT')) {
          
      for (k in 1:15) {
      
        earth_BI_obj_ijk <- vector('list', 2)
        
        BIModelOutputSS <- subset(BIModelOutput, CTAbbr == i & SSAbbr == j & Timestep == k * 10)
        BIModelOutputSS <- merge(BIModelOutputSS, inputs[[i]], by.x=1, by.y=0)
        
        if(verbose) cat('Model: ')
        
        earth_BI_obj_ijk[[1]] <- earth(qlogis(((Area / (1000 / 3)) * .998) + .001) ~ ., 
          data=BIModelOutputSS[-1:-4], degree=3)
        
        earth_BI_obj_ijk[[1]]$fitted.values <- NA
        
        if(verbose) cat(sprintf('%s %s %s \n', i, j, k))
        
        earth_BI_obj_ijk[[2]] <- mclapply(integer(n_boot), eval.parent(substitute(function(...) {
          x <- earth(qlogis(((Area / (1000 / 3)) * .998) + .001) ~ ., 
            data=BIModelOutputSS[sample(seq_len(nrow(BIModelOutputSS)), replace=TRUE), -1:-4], 
            degree=3)
          x$fitted.values <- NA
          x
          })), mc.cores=detectCores() / ceiling(n_boot / detectCores()))
        
        save('earth_BI_obj_ijk', file=sprintf('%s/%s_%s_%s.rda', dir, i, j, k))
        
      }
    
    }
      
  }
  
}  

