sensitivity_BI <- function(dir, newdata, manage, param, n, verbose=FALSE) {

  sensitivity_BI_obj <- vector('list', n); gc(FALSE)
  param_values <- seq(
    min(newdata[[manage]][, param]),
    max(newdata[[manage]][, param]),
    length=n)

  for (i in seq_len(n)) {

    inputs <- newdata
    inputs[[manage]][, param] <- param_values[i]

    predict_BI_obj <- vector('list', 3); gc(FALSE)
    names(predict_BI_obj) <- c('LDW', 'HDW', 'MAT')

    for (j in names(predict_BI_obj)) {

      predict_BI_obj_j <- vector('list', 15); gc(FALSE)

      for (k in seq_along(predict_BI_obj_j) ) {

        predict_BI_obj_jk <- vector('list', 2); gc(FALSE)

        if(verbose) cat('Prediction: ')

        load(sprintf('%s/%s_%s_%s.rda', dir, manage, j, k))

        predict_BI_obj_jk[[1]] <- plogis(predict(earth_BI_obj_ijk[[1]],
          inputs[[manage]])); gc(FALSE)

        if(verbose) cat(sprintf('%s %s %s %s %s \n', param, i, manage, j, k))

        predict_BI_obj_jk[[2]] <- sapply(seq_along(earth_BI_obj_ijk[[2]]), function(REP) {
          plogis(predict(earth_BI_obj_ijk[[2]][[REP]],
            inputs[[manage]]))}); gc(FALSE)

        rm(earth_BI_obj_ijk)

        predict_BI_obj_j[[k]] <- predict_BI_obj_jk; gc(FALSE)

        }

      predict_BI_obj[[j]] <- predict_BI_obj_j; gc(FALSE)

    }

    sensitivity_BI_obj[[i]] <- predict_BI_obj

  }

  attr(sensitivity_BI_obj, 'tested_values') <- param_values
  return(sensitivity_BI_obj)

