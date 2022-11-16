# Split by maturity

# run index.Rmd to get all data loaded.

# explore raw data
bind_rows(survey_samples, commercial_samples) %>% filter(maturity_code < 8 & maturity_code > 0) %>%
  ggplot() + geom_boxplot(aes(as.factor(maturity_code), age, colour = survey_abbrev), alpha = 0.2) +
  ylim(0,25)
# facet_wrap(~survey_abbrev)

bind_rows(survey_samples, commercial_samples) %>%
  filter(age < 30 & length < 70) %>%
  filter(maturity_code < 8 & maturity_code > 0) %>% mutate(maturity_code = ifelse(maturity_code %in% c(4,5), "4-5", maturity_code)) %>%
  ggplot() + geom_jitter(aes(as.factor(maturity_code), age
                             , colour = length
  ), size = 3, alpha = 0.5, shape = 20) +
  scale_colour_viridis_c() +
  ylim(0,25)

all_samples <- bind_rows(survey_samples, commercial_samples) %>% filter(maturity_code %in%c(1,3,4,5,6))

gfiscamutils:::export_mat_lw_age(all_samples,
                   surv_abbrevs = c(
                     "HS MSA",     "MSSM QCS"  ,  "MSSM WCVI",
                     "SYN QCS",
                                    "SYN WCVI",
                                    "SYN HS",
                                    "SYN WCHG"),
                   # areas here are 3CD and 5ABCDE in order
                   areas = c("03",
                             "04",
                             "05",
                             "06",
                             "07",
                             "08",
                             "09"))

gfiscamutils:::export_mat_lw_age(survey_samples,
                                 surv_abbrevs = c(
                                   "HS MSA",     "MSSM QCS"  ,  "MSSM WCVI",
                                   "SYN QCS",
                                   "SYN WCVI",
                                   "SYN HS",
                                   "SYN WCHG"),
                                 # areas here are 3CD and 5ABCDE in order
                                 areas = c("03",
                                           "04",
                                           "05",
                                           "06",
                                           "07",
                                           "08",
                                           "09"))


# without resting
survey_samples2 <- survey_samples %>% filter(maturity_code %in%c(1,3,4,5,6))
survey_samples_syn2 <- survey_samples_syn %>% filter(maturity_code %in%c(1,3,4,5,6))
commercial_samples2 <- commercial_samples %>% filter(maturity_code %in%c(1,3,4,5,6))

unique(survey_samples$survey_abbrev)
# check for duplication in sample_ids between datasets
sum(!is.na(match(commercial_samples$sample_id, survey_samples$sample_id)))
survey_samples_syn2 <- bind_rows(survey_samples2, commercial_samples2)


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
  # tr("Coastwide synoptic trawl surveys"),
  tr("Coastwide survey and commercial data"),
  "")

plot_mat_panels <- function(fit_obj){
  # p <- plot_mat_annual_ogives(fit_obj, rug_n = 2500
  # ) +
    p <- plot_mat_ogive(fit_obj,
            col = c("F" = "#FF0000", "M" = "#0000FF")
            ) +
    guides(colour = "none", fill = "none", lty = "none") +
    ggplot2::guides(#lty = "none",
                    colour = "none") +
    # scale_colour_viridis_d(option = "plasma", direction = -1) +
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

  plot_mat_panels(mat_lst[[7]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    guides(colour = "none", fill = "none", lty = "none") +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[7]]),

  plot_mat_panels(mat_lst[[8]]) +
    coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
    theme(axis.title = element_blank()) +
    ggtitle(mat_title_lst[[8]]),

  plot_mat_panels(mat_lst[[9]]) +
    coord_cartesian(xlim = c(0, 18), ylim = c(0, 1)) +
    guides(colour = "none", fill = "none", lty = "none") +
    ggtitle(mat_title_lst[[9]]) +
    xlab(tr("Age (years)")),

  plot_mat_panels(mat_lst[[10]]) +
    coord_cartesian(xlim = c(0, 82), ylim = c(0, 1)) +
    ggtitle(mat_title_lst[[10]]) +
    guides(colour = "none", fill = "none", lty = "none") +
    # scale_colour_viridis_d(option = "C") +
    xlab(paste0(tr("Length"), " (cm)"))
)

grid.arrange(grobs = ma_plot_lst,
             ncol = 2,
             widths = c(1,1.2),
             left = paste0(tr("Probability mature")))



