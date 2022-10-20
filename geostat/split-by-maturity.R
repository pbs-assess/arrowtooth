# run index.Rmd to get all data loaded.

# Split by maturity
dat_split <- gfplot::split_catch_maturity(
  survey_sets = dat$survey_sets,
  fish = dat$survey_samples, survey = c("SYN HS", "SYN QCS", "SYN WCVI"),
  p_threshold = 0.95,
  use_median_ratio = TRUE, sample_id_re = FALSE
)

dat_split2 <- gfplot::split_catch_maturity(
  survey_sets = dat$survey_sets,
  fish = dat$survey_samples, survey = c("SYN HS", "SYN QCS", "SYN WCVI"),
  p_threshold = 0.05,
  use_median_ratio = TRUE, sample_id_re = FALSE
)


dat_split$data


ggplot(filter(dat_split$data, adult_density > 0.0001)) +
  geom_point(aes(longitude, latitude,
    # size = adult_density,
    alpha = adult_density
  ), colour = "blue") +
  xlim(-134, -124.7) +
  ylim(48, 54.5) +
  theme(legend.position = c(0.2, 0.2))

ggplot(filter(dat_split2$data, imm_density > 0.00001)) +
  geom_point(aes(longitude, latitude,
    # size = imm_density,
    alpha = imm_density
  ), colour = "red") +
  xlim(-134, -124.7) +
  ylim(48, 54.5) +
  theme(legend.position = c(0.2, 0.2))



# # Time-varying maturity split

# mat_fn <- file.path(data_dir, "fit_mat_ogive_output.rds")
# if(file.exists(mat_fn)){
#   mat_lst <- readRDS(mat_fn)
# }else{
  mat_lst <- list(
    fit_mat_ogive(filter(survey_samples, survey_abbrev %in% "SYN QCS"), sample_id_re = T, year_re = T),
    fit_mat_ogive(filter(survey_samples, survey_abbrev %in% "SYN QCS"), sample_id_re = T, year_re = T, type = "length"),
    fit_mat_ogive(filter(survey_samples, survey_abbrev %in% "SYN HS"), sample_id_re = T, year_re = T),
    fit_mat_ogive(filter(survey_samples, survey_abbrev %in% "SYN HS"), sample_id_re = T, year_re = T, type = "length"),
    fit_mat_ogive(filter(survey_samples, survey_abbrev %in% "SYN WCVI"), sample_id_re = T, year_re = T),
    fit_mat_ogive(filter(survey_samples, survey_abbrev %in% "SYN WCVI"), sample_id_re = T, year_re = T, type = "length"),
    fit_mat_ogive(commercial_samples, sample_id_re = T, year_re = T),
    fit_mat_ogive(commercial_samples, sample_id_re = T, year_re = T, type = "length"),    fit_mat_ogive(survey_samples_syn, sample_id_re = T, year_re = T),
    fit_mat_ogive(survey_samples_syn, sample_id_re = T, year_re = T, type = "length"))
#   saveRDS(mat_lst, mat_fn)
# }
mat_title_lst <- list(
  tr("SYN QCS (5AB)"),
  "",
  tr("SYN HS (5CD)"),
  "",
  tr("SYN WCVI (3CD)"),
  "",
  tr("Commercial"),
  "",
  tr("Coastwide synoptic trawl surveys"),
  "")

plot_mat_panels <- function(fit_obj){
  p <- plot_mat_annual_ogives(fit_obj
  ) +
    # p <- plot_mat_ogives(fit_obj,
    #         col = c("F" = "#FF0000", "M" = "#0000FF")
    #         ) +
    guides(colour = "none", fill = "none", lty = "none") +
    ggplot2::guides(lty = "none", colour = "none") +
    theme(axis.title.y = element_blank())
  p
}

ma_plot_lst <- list(
  plot_mat_panels(mat_lst[[1]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[1]]),

  plot_mat_panels(mat_lst[[2]]) +
    coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[2]]),

  plot_mat_panels(mat_lst[[3]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[3]]),

  plot_mat_panels(mat_lst[[4]]) +
    coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[4]]),

  plot_mat_panels(mat_lst[[5]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[5]]),

  plot_mat_panels(mat_lst[[6]]) +
    coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[6]]),

  plot_mat_panels(mat_lst[[7]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[7]]),

  plot_mat_panels(mat_lst[[8]]) +
    coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[8]]),

  plot_mat_panels(mat_lst[[9]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    ggtitle(mat_title_lst[[9]]) +
    xlab(tr("Age (years)")),

  plot_mat_panels(mat_lst[[10]]) +
    coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
    ggtitle(mat_title_lst[[10]]) +
    # scale_colour_viridis_d(option = "C") +
    xlab(paste0(tr("Length"), " (cm)"))
)

grid.arrange(grobs = ma_plot_lst,
             ncol = 2,
             left = paste0(tr("Probability mature")))