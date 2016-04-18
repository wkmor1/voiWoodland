library(voiWoodland)
library(dplyr)
library(ggplot2)

evoi_df <-
  evoi(
    outcome         = bifaw$outcome, 
    action          = bifaw$action,
    parameter       = bifaw$parameter,
    parameter_value = bifaw$parameter_value,
    run             = bifaw$run,
    sample          = bifaw$sample,
    ncores = 50
  )


evdf <-
  regexec(
    "([A-Za-z]*)([A-Z]{4})to([A-Z]{4})given([A-Za-z]*)then([A-Za-z]*)",
    unique(bifaw$parameter)
  ) %>%
  regmatches(unique(bifaw$parameter), .) %>%
  do.call(rbind, .) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(c("parameters", "type", "from", "to", "action", "cause")) %>%
  merge(evoi_df) %>%
  filter(evwpxi > evwoi, type == "Probability") %>%
  mutate(evpi = evwpi - evwoi, evpxi = evwpxi - evwoi) 

top_params <-
  with(evdf, tapply(evpxi, list(parameters, action), max)) %>%
  {rownames(.)[apply(., 2, which.max)]}

evdf <- filter(evdf, parameters %in% top_params)

evoi_gg <-
  ggplot(evdf) +
  geom_line(
    aes(y = evpi, color = NULL, linetype = NULL),
    distinct(select(evdf, evpi, max_eco, action)),
    size = 1.25,
    col = "grey"
  ) +
  aes(x = max_eco, y = evpxi, linetype = action) +
  scale_linetype_discrete(name = "Action", guide = guide_legend(keywidth = 2)) +
  scale_x_continuous(
    "Maximum allowed ecological thinning",
    labels = scales::percent
  ) +
  scale_y_continuous("EVI", labels = scales::percent) +
  geom_line(size = 1.25) +
  theme_bw()
