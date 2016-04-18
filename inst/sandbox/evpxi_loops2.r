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

nb_theta_i <- array(dim = c(125, 125, 4))
max_nb_theta_i <- matrix(nrow = 125, ncol = 125)
mean_nb_theta_i <- matrix(nrow = 125, ncol = 4)
max_mean_nb_theta_i <- mean_max_nb_theta_i <- vector("numeric", 125)

for (i in 1:125) {
  for (j in 1:125) {
    predictors             <- parameters[[action]][j, ]
    predictors[theta]      <- parameters[[action]][[theta]][i]
    value                  <- earth:::predict.earth(models[[action]], predictors)
    nb_theta_i[i, j, 1:2]  <- as.numeric(Area[j, 1:2])
    nb_theta_i[i, j, 3]    <- as.numeric(value * max_eco + Area[j, 1] * (1 - max_eco))
    nb_theta_i[i, j, 4]    <- as.numeric(value * max_eco + Area[j, 2] * (1 - max_eco))
    max_nb_theta_i[i, j]   <- max(nb_theta_i[i, j, ])
  }
  mean_nb_theta_i[i, ]   <- colMeans(nb_theta_i[i, , ])
  mean_max_nb_theta_i[i] <- mean(max_nb_theta_i[i, ])
  max_mean_nb_theta_i[i] <- max(mean_nb_theta_i[i, ])
}

mean_mean_nb_theta_i <- colMeans(mean_nb_theta_i)
max_mean_mean_nb_theta_i <- max(mean_mean_nb_theta_i)
mean_mean_max_nb_theta_i <- mean(mean_max_nb_theta_i)
mean_max_mean_nb_theta_i <- mean(max_mean_nb_theta_i)

pevpi1 <- evpi - (mean_mean_max_nb_theta_i - mean_max_mean_nb_theta_i)

pevpi2 <- mean_max_mean_nb_theta_i - max_mean_nb

pevpi3 <- mean_max_mean_nb_theta_i - max_mean_mean_nb_theta_i

