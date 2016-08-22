#' Expected Value of Information
#'
#' Calculate the expected value of information for box ironbark and woodland 
#' management
#'
#' @param data Input and output data as a single data.frame.
#' @param ncores Interger. Number of cpu cores.
#' @param max_eco Numeric. Maximum proportion of the landscape allocatable to 
#'        thinning.
#' @param type Character. Type of evoi analysis.
#' @param parameter_list list. When \code{type = "evpxi_list"} a list of
#'        parameter sets must be supplied where the list elements are named
#'        vectors of parameter where the names correspond to the action the
#'        parameter of interest associated with.
#'
#' @importFrom dplyr arrange distinct select_
#' @importFrom parallel mclapply
#' @importFrom tidyr spread_
#' @importFrom stats predict
#' @importFrom earth earth

#' @export

evoi <-
  function(
    data, ncores = 1, max_eco = .2, 
    type = c("evpi", "evwoi", "evpxi_list", "evpxi"),
    parameter_list = NULL
  ) {

    type <- match.arg(type)
    
    with(
      data,
      {
        actions <- unique(action)

        data <-
          lapply(
            actions,
            function(x) {
              data <-
                spread_(
                  data[data[, "action"] == x, ],
                  "parameter",
                  "parameter_value"
                )
              data  <- arrange(data, sample, run)
              data[, setdiff(colnames(data), c("action", "run", "sample"))]
            }
          )

        models <- lapply(data, fit_earth, "outcome")
        parameters <- lapply(data, select_, paste0("-", "outcome"))
        parameters <- lapply(parameters, distinct)
        outcomes <- mapply(predict, models, parameters)
        n_samples <- length(unique(sample))
        colnames(outcomes) <- names(models) <- names(parameters) <- actions

        out <-
          lapply(
            max_eco,
            evoi_,
            outcomes,
            parameters,
            models,
            n_samples,
            actions,
            ncores,
            type,
            parameter_list
          )

        do.call(rbind, out)
      }
    )
  }

o2v <- function(outcomes, max_eco) {
  with(
    as.data.frame(outcomes),
    data.frame(
      NM     = NM,
      HF     = HF,
      EN     = ET * max_eco + NM * (1 - max_eco),
      EH     = ET * max_eco + HF * (1 - max_eco)
    )
  )
}

calc_vwpxi <-
  function(sample, theta, action, parameters, models, max_eco) {
    for (i in seq_along(action)) {
      parameter_action <- getElement(parameters, action[i])
      parameter_action[, theta[i]] <- parameter_action[sample, theta[i]]
      parameters <- within(parameters, assign(action[i], parameter_action))
    }
    outcomes <- mapply(predict, models, parameters)
    colnames(outcomes) <- names(models)
    value <- o2v(outcomes, max_eco)
    mean_value <- colMeans(value)
    ans <- max(mean_value)
    attr(ans, "which.max") <- which.max(mean_value)
    ans
  }

calc_evwpxi <-
  function(theta, action, n_samples, parameters, models, max_eco) {

    results <-
      sapply(
        seq_len(n_samples),
        calc_vwpxi,
        theta,
        action,
        parameters,
        models,
        max_eco
      )

    list(evwpxi = mean(results), vwpxi = results)

  }

calc_evwpxi_ <-
  function(action, n_theta, n_samples, parameters, models, max_eco, ncores) {
    mclapply(
      seq_len(n_theta[[action]]),
      calc_evwpxi,
      action,
      n_samples,
      parameters,
      models,
      max_eco,
      mc.cores = ncores
    )
  }

evoi_ <-
  function(
    max_eco, outcomes, parameters, models, n_samples, actions,
    ncores, type, parameter_list
  ) {

    value <- o2v(outcomes, max_eco)

    evwoi <- max(colMeans(value))

    switch(
      type,

      evpi =
        {
          evwpi <- mean(apply(value, 1, max))

          attr(evwpi, "vwpi") <- apply(value, 1, which.max)

          data.frame(
            parameters = "EVPI",
            max_eco    = max_eco,
            evi        = evwpi - evwoi,
            stringsAsFactors = FALSE
          )
        },

      evwoi = 
        data.frame(
          parameters = "EVWOI",
          max_eco    = max_eco,
          evi        = evwoi,
          stringsAsFactors = FALSE
        ),

      evpxi_list =
        {
          evwpxi <-
            mapply(
              calc_evwpxi,
              parameter_list,
              lapply(parameter_list, names),
              MoreArgs =
                list(
                  n_samples  = n_samples,
                  parameters = parameters,
                  models     = models,
                  max_eco    = max_eco
                ),
              SIMPLIFY = FALSE
            )

          ans <-
            data.frame(
              parameters = names(parameter_list),
              max_eco    = max_eco,
              evi        =
                unlist(lapply(evwpxi, getElement, name = "evwpxi")) - evwoi,
              stringsAsFactors = FALSE
            )

          attr(ans, "vwpxi") <- lapply(evwpxi, getElement, name = "vwpxi")

          ans
        },

      evpxi = 
        {
          evwpxi <-
            lapply(
              actions,
              calc_evwpxi_,
              lapply(parameters, ncol),
              n_samples,
              parameters,
              models,
              max_eco,
              ncores
            )

          ans <-
            data.frame(
              parameters = unlist(lapply(parameters, names)),
              max_eco    = max_eco,
              evi        = 
                unlist(
                  lapply(
                    evwpxi, 
                    function(x) lapply(x, getElement, name = "evwpxi")
                  )
                ) - evwoi,
              stringsAsFactors = FALSE
            )

          attr(ans, "vwpxi") <-
            lapply(
              evwpxi,
              function(x) lapply(x, getElement, name = "vwpxi")
            )

          ans

        }
    )
  }

fit_earth <-
  function(x, outcome, ...) {
    earth(
      x      = x[, setdiff(colnames(x), outcome)],
      y      = x[, outcome],
      degree = 5,
      ...
    )
  }
