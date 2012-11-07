predict_BI <- function(newdata, dir, verbose=FALSE) {
    
  predict_BI_obj <- vector('list', 3); gc(FALSE)
  names(predict_BI_obj) <- c('BIW', 'BIMan', 'BIEco')
  
  for (i in names(predict_BI_obj)) {
        
    predict_BI_obj_i <- vector('list', 3); gc(FALSE)
    names(predict_BI_obj_i) <- c('LDW', 'HDW', 'MAT')
        
    for (j in names(predict_BI_obj_i)) {
      
      predict_BI_obj_ij <- vector('list', 15); gc(FALSE)
      
      for (k in seq_along(predict_BI_obj_ij) ) {
        
        predict_BI_obj_ijk <- vector('list', 2); gc(FALSE)
         
        if(verbose) cat('Prediction: ')
        
        load(sprintf('%s/%s_%s_%s.rda', dir, i, j, k))
        
        predict_BI_obj_ijk[[1]] <- ((plogis(predict(earth_BI_obj_ijk[[1]], 
          newdata[[i]])) - .001) / .998); gc(FALSE)
        
        if(verbose) cat(sprintf('%s %s %s \n', i, j, k))
        
        predict_BI_obj_ijk[[2]] <- sapply(seq_along(earth_BI_obj_ijk[[2]]), function(REP) {
          ((plogis(predict(earth_BI_obj_ijk[[2]][[REP]], 
          newdata[[i]])) - .001) / .998)}); gc(FALSE)
        
        rm(earth_BI_obj_ijk)
        
        predict_BI_obj_ij[[k]] <- predict_BI_obj_ijk; gc(FALSE)
        
      }
      
      predict_BI_obj_i[[j]] <- predict_BI_obj_ij; gc(FALSE)
      
    }
    
    predict_BI_obj[[i]] <- predict_BI_obj_i; gc(FALSE)
    
  }
  
  return(predict_BI_obj)
  
}  
