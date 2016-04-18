library(dplyr)

load("data/bifaw.rda")

set.seed(0)

max_eco <- .2

Area <- lapply(bifaw, `[[`, "Area") %>% data.frame

Area2 <-
  lapply(bifaw, `[[`, "Area") %>%
  data.frame %>%
  mutate(
    EcoW   = BIEco * max_eco + BIW * (1 - max_eco),
    EcoMan = BIEco * max_eco + BIMan * (1 - max_eco),
    BIEco = NULL
  )

max_mean_nb <- Area2 %>% colMeans %>% max
mean_max_nb <- Area2 %>% apply(1, max) %>% mean
evpi <- mean_max_nb - max_mean_nb

models <-
  lapply(bifaw,
         function(x) {
           earth::earth(
             formula = Area ~ .,
             data    = x,
             degree  = 5,
             pmethod = "none"
           )
         }
  )

parameters <- lapply(bifaw, select_, "-Area") %>% lapply(distinct)

n_samples <- lapply(parameters, nrow)

n_theta <- lapply(parameters, ncol)

action <- "BIEco"

theta <- 5

library(foreach)

nb_theta_i <-
  foreach(i = 1:125) %do%
      {
        predictors             <- parameters[[action]]
        predictors[theta]      <- parameters[[action]][[theta]][i]
        value                  <- earth:::predict.earth(models[[action]], predictors)
        value                  <- rep(value, each = 10)
        switch(
          action,
          BIW   = 
            data.frame(
              BIW    = value,
              BIMan  = Area[, "BIMan"],
              EcoW   = Area[, "BIEco"] * max_eco + value * (1 - max_eco),
              EcoMan = Area[, "BIEco"] * max_eco + Area[, "BIMan"] * (1 - max_eco)
            ),
          BIMan = 
            data.frame(
              BIW    = Area[, "BIW"],
              BIMan  = value, 
              EcoW   = Area[, "BIEco"] * max_eco + Area[, "BIW"] * (1 - max_eco),
              EcoMan = Area[, "BIEco"] * max_eco, + value * (1 - max_eco)
            ),
          BIEco =
            data.frame(
              BIW    = Area[, "BIW"],
              BIMan  = Area[, "BIMan"],
              EcoW   = value * max_eco + Area[, "BIW"] * (1 - max_eco),
              EcoMan = value * max_eco + Area[, "BIMan"] * (1 - max_eco)
            )
        )
      }


