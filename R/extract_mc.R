# # Extract mcmc posteriors from the base model for Arrowtooth
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(tidylog, warn.conflicts = FALSE)
# debug <- TRUE
# if(!debug){
#   devtools::unload("tidylog")
# }
#
# nongit_dir <- file.path(dirname(here::here()), "arrowtooth-nongit")
# base_model <- readRDS(file.path(nongit_dir, "models", "01-base", "01-base.rds"))
# lrp <- base_model$mcmc$params.est %>% as_tibble() %>%
#   select("bmsy") %>%
#   mutate(iter = seq_len(n()),
#          lrp = 0.4 * bmsy)
# base_model$mcmc$sbt[[1]] %>%
#   tidyr::pivot_longer(everything(), names_to = "year", values_to = "ssb") %>%
#   mutate(iter = rep(seq_len(2000), each = length(unique(year)))) %>%
#   arrange(year) %>%
#   mutate(year = as.numeric(year)) %>%
#   left_join(lrp) %>%
#   ggplot(aes(year, ssb / lrp, group = iter)) + geom_line(alpha = 0.05)
#
#
# j <- base_model$mcmc$sbt[[1]] %>% as_tibble()
