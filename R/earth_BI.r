earth_BI <- function(BI_output, BI_input, n_boot, dir, verbose=FALSE) {
    
  for (i in names(BI_input)) {
      
    for (j in c('LDW', 'HDW', 'MAT')) {
          
      for (k in 1:15) {
      
        earth_BI_obj_ijk <- vector('list', 2)
        
        BI_outputSS <- subset(BI_output, CTAbbr == i & SSAbbr == j & Timestep == k * 10)
        BI_outputSS <- merge(BI_outputSS, BI_input[[i]], by.x=1, by.y=0)
        
        if(verbose) cat('Model: ')
        
        earth_BI_obj_ijk[[1]] <- earth(qlogis(((Area / (1000 / 3)) * .998) + .001) ~ ., 
          data=BI_outputSS[-1:-4], degree=3)
        
        earth_BI_obj_ijk[[1]]$fitted.values <- NA
        
        if(verbose) cat(sprintf('%s %s %s \n', i, j, k))
        
        earth_BI_obj_ijk[[2]] <- mclapply(integer(n_boot), eval.parent(substitute(function(...) {
          x <- earth(qlogis(((Area / (1000 / 3)) * .998) + .001) ~ ., 
            data=BI_outputSS[sample(seq_len(nrow(BI_outputSS)), replace=TRUE), -1:-4], 
            degree=3)
          x$fitted.values <- NA
          x
          })), mc.cores=detectCores() / ceiling(n_boot / detectCores()))
        
        save('earth_BI_obj_ijk', file=sprintf('%s/%s_%s_%s.rda', dir, i, j, k))
        
      }
    
    }
      
  }
  
}  

