# Split by maturity

# run index.Rmd to get all data loaded.

# without resting
survey_samples2 <- survey_samples %>% filter(maturity_code < 7)
survey_samples_syn2 <- survey_samples_syn %>% filter(maturity_code < 7)
commercial_samples2 <- commercial_samples %>% filter(maturity_code < 7)

fit_obj <- fit_mat_ogive(survey_samples_syn2,
                         # sample_id_re = T,
                         type = "age")

p <- plot_mat_ogive(fit_obj,
          col = c("F" = "#FF0000", "M" = "#0000FF")
          ) +
  guides(colour = "none", fill = "none", lty = "none") +
  ggplot2::guides(lty = "none", colour = "none") +
  # scale_colour_viridis_d(option = "plasma") +
  theme(axis.title.y = element_blank())
p


# # Time-varying maturity split

mat_lst <- list(
  fit_mat_ogive(filter(survey_samples2, survey_abbrev %in% "SYN QCS"), sample_id_re = T, year_re = T),
  fit_mat_ogive(filter(survey_samples2, survey_abbrev %in% "SYN QCS"), sample_id_re = T, year_re = T, type = "length"),
  fit_mat_ogive(filter(survey_samples2, survey_abbrev %in% "SYN HS"), sample_id_re = T, year_re = T),
  fit_mat_ogive(filter(survey_samples2, survey_abbrev %in% "SYN HS"), sample_id_re = T, year_re = T, type = "length"),
  fit_mat_ogive(filter(survey_samples2, survey_abbrev %in% "SYN WCVI"), sample_id_re = T, year_re = T),
  fit_mat_ogive(filter(survey_samples2, survey_abbrev %in% "SYN WCVI"), sample_id_re = T, year_re = T, type = "length"),
  fit_mat_ogive(commercial_samples2, sample_id_re = T, year_re = T),
  fit_mat_ogive(commercial_samples2, sample_id_re = T, year_re = T, type = "length"),
  fit_mat_ogive(survey_samples_syn2, sample_id_re = T, year_re = T),
  fit_mat_ogive(survey_samples_syn2, sample_id_re = T, year_re = T, type = "length")
  )

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
    # guides(colour = "none", fill = "none", lty = "none") +
    # ggplot2::guides(#lty = "none",
    #                 colour = "none") +
    scale_colour_viridis_d(option = "plasma") +
    theme(axis.title.y = element_blank())
  p
}

ma_plot_lst <- list(
  plot_mat_panels(mat_lst[[1]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    guides(colour = "none", fill = "none", lty = "none") +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[1]]),

  plot_mat_panels(mat_lst[[2]]) +
    coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[2]]),

  plot_mat_panels(mat_lst[[3]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    guides(colour = "none", fill = "none", lty = "none") +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[3]]),

  plot_mat_panels(mat_lst[[4]]) +
    coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[4]]),

  plot_mat_panels(mat_lst[[5]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    guides(colour = "none", fill = "none", lty = "none") +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[5]]),

  plot_mat_panels(mat_lst[[6]]) +
    coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[6]]),
  #
  # plot_mat_panels(mat_lst[[7]]) +
  #   coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
  #   guides(colour = "none", fill = "none", lty = "none") +
  #   theme(axis.title = element_blank()) +
  #   ggtitle(mat_title_lst[[7]]),
  #
  # plot_mat_panels(mat_lst[[8]]) +
  #   coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
  #   theme(axis.title = element_blank()) +
  #   ggtitle(mat_title_lst[[8]]),

  plot_mat_panels(mat_lst[[9]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    guides(colour = "none", fill = "none", lty = "none") +
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
