sens_prepost_BI <- function(dir, newdata, manage, param, n, verbose=FALSE) {
  
  sensitivity_BI_obj <- vector('list', n)
  preposts <- pre_posterior(newdata[[manage]][, param], n, 1000)

  for (i in seq_len(n)) {
    
    inputs <- newdata
    inputs[[manage]][, param] <- preposts[[i]]
      
    predict_BI_obj <- vector('list', 3)
    names(predict_BI_obj) <- c('LDW', 'HDW', 'MAT')
      
    for (j in names(predict_BI_obj)) {
      
      if(verbose) cat(sprintf('%s %s %s %s \n', param, i, manage, j))
      
      predict_BI_obj[[j]] <- mclapply(1:15, function(k) {
          
        predict_BI_obj_jk <- vector('list', 2)
          
        load(sprintf('%s/%s_%s_%s.rda', dir, manage, j, k))
          
        predict_BI_obj_jk[[1]] <- ((plogis(predict(earth_BI_obj_ijk[[1]], 
          inputs[[manage]])) - .001) / .998)
          
        predict_BI_obj_jk[[2]] <- sapply(seq_along(earth_BI_obj_ijk[[2]]), 
          function(REP) {
            ((plogis(predict(earth_BI_obj_ijk[[2]][[REP]], 
              inputs[[manage]])) - .001) / .998)
            })
          
        return(predict_BI_obj_jk)
        
      }, mc.cores=5)
              
    }
    
    sensitivity_BI_obj[[i]] <- predict_BI_obj
    
  }
  
  attr(sensitivity_BI_obj, 'tested_values') <- preposts
  return(sensitivity_BI_obj)

}