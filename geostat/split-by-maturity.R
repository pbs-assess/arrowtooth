# run index.Rmd to get all data loaded.

# Split by maturity
dat_split <- gfplot::split_catch_maturity(survey_sets = dat$survey_sets,
  fish = dat$survey_samples, survey = c("SYN HS", "SYN QCS", "SYN WCVI"),
  p_threshold = 0.95,
  use_median_ratio = TRUE, sample_id_re = FALSE)

dat_split2 <- gfplot::split_catch_maturity(survey_sets = dat$survey_sets,
  fish = dat$survey_samples, survey = c("SYN HS", "SYN QCS", "SYN WCVI"),
  p_threshold = 0.05,
  use_median_ratio = TRUE, sample_id_re = FALSE)


dat_split$data


ggplot(filter(dat_split$data, adult_density > 0.0001)) +
  geom_point(aes(longitude, latitude,
                 # size = adult_density,
                 alpha = adult_density), colour = "blue") +
  xlim(-134,-124.7) +  ylim(48,54.5) +
  theme(legend.position = c(0.2,0.2))

ggplot(filter(dat_split2$data, imm_density > 0.00001)) +
  geom_point(aes(longitude, latitude,
                 # size = imm_density,
                 alpha = imm_density), colour = "red") +
  xlim(-134,-124.7) +  ylim(48,54.5) +
  theme(legend.position = c(0.2,0.2))


