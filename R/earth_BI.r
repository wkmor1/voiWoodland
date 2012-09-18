earth_BI <- function(n_boot, BIModelOutput, inputs, verbose=FALSE) {
  
  attach(inputs)
  
  earth_BI_obj <- vector('list', length(inputs))
  names(earth_BI_obj) <- names(inputs)
  
  for (i in names(earth_BI_obj)) {
   
    earth_BI_obj_i <- vector('list', 3)
    names(earth_BI_obj_i) <- c('LDW', 'HDW', 'MAT')
      
    for (j in names(earth_BI_obj_i)) {
    
      earth_BI_obj_ij <- vector('list', 15)
      
      for (k in seq_along(earth_BI_obj_ij) ) {
      
        earth_BI_obj_ijk <- vector('list', 2)
        
        BIModelOutputSS <- subset(BIModelOutput, CTAbbr == i & SSAbbr == j & Timestep == k * 10)
        BIModelOutputSS <- merge(BIModelOutputSS, get(sprintf('%sParams', i)), by.x=1, by.y=0)
        
        if(verbose) cat('Model: ')
        
        earth_BI_obj_ijk[[1]] <- earth(Area ~ ., data=BIModelOutputSS[-1:-4], degree=3)
        
        earth_BI_obj_ijk[[1]]$fitted.values <- NA
        
        if(verbose) cat(sprintf('%s %s %s \n', i, j, k))
        
        earth_BI_obj_ijk[[2]] <- mclapply(integer(n_boot), eval.parent(substitute(function(...) {
          x <- earth(Area ~ ., 
            data=BIModelOutputSS[sample(seq_len(nrow(BIModelOutputSS)), replace=TRUE), -1:-4], 
            degree=3)
          x$fitted.values <- NA
          x
          })), mc.cores=detectCores() / ceiling(n_boot / detectCores()))
        
        earth_BI_obj_ij[[k]] <- earth_BI_obj_ijk
        
      }

      earth_BI_obj_i[[j]] <- earth_BI_obj_ij
    
    }
    
    earth_BI_obj[[i]] <- earth_BI_obj_i
  
  }
  
  return(earth_BI_obj)
  
}  

