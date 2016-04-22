library(voiWoodland)
library(plyr)
library(dplyr)
library(tidyr)

bifaw <-
  left_join(
    {
      filter(BI_output, Timestep == 150, grepl("HDW|MAT", SSAbbr)) %>%
        mutate(run = paste(CTAbbr, Project, rep(1:10, length.out = nrow(.)))) %>%
        group_by(run) %>%
        summarise(outcome = sum(Area)) %>%
        separate(run, c("action", "sample", "run")) %>%
        ungroup
    },
    {
      lapply(BI_input, t) %>%
        lapply(data.frame) %>%
        do.call(rbind, .) %>% 
        add_rownames() %>%
        gather(sample, parameter_value, -rowname) %>%
        mutate(sample = sub("X", "", sample)) %>%
        separate(rowname, c("action", "parameter"))
    }
  ) %>%
  mutate(
    run = as.integer(run), 
    sample = as.integer(factor(sample)),
    action = revalue(action, c("BIEco" = "ET", "BIW" = "NM", "BIMan" = "HF"))
  ) %>%
  select(action, parameter, sample, parameter_value, run, outcome) %>%
  arrange(action, parameter, sample, run)

bifaw$parameter <-
  bifaw$parameter %>%
  sub("BIEco", "ET", .) %>%
  sub("BIW", "NM", .) %>%
  sub("BIMan", "HF", .) %>%
  sub("WNonLethal", "Windthrow", .) %>%
  gsub("HDW", "MHDW", .) %>%
  gsub("LDW", "LDRG", .) %>%
  gsub("MAT", "MLDW", .) %>%
  gsub("REG", "HDRG", .)

bifaw <-
  list(
    inputs =
      distinct(select(bifaw, action, parameter, sample, parameter_value)),
    outputs =
      distinct(select(bifaw, action, sample, run, outcome))
  )

save(bifaw, file = "data/bifaw.rda")
