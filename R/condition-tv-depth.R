#' Create a plot of effect of time-varying third-order polynomial of depth
#' on condition
#' @param fit_lst A list of a fit from [sdmTMB] and a data frame
#' of the data used in that fit as returned by [condition_fit()]
#' @param trawl_temp_fn The trawl temperature data frame RDS filename
#'
#' @returns A [ggplot2::ggplot()] object
#' @export
condition_tv_depth <- \(fit_lst,
                        trawl_temp_fn = "/srv/arrowtooth/arrowtooth-nongit/data/trawl_temp.rds"){

  if(!file.exists(trawl_temp_fn)){
    bail("File `", trawl_temp_fn, "` does not exist.")
  }

  td <- readRDS(trawl_temp_fn)

  fit <- fit_lst$fit
  ds <- fit_lst$ds

  mesh <- make_mesh(ds, c("X", "Y"), cutoff = 20)

  fit_tv <- sdmTMB(cond_fac ~ 0 + s(year),
                   time_varying = ~ 0 + poly(log_depth, 3, raw = T),
                   mesh = mesh,
                   data = ds,
                   # spatial = "off",
                   spatial = "on",
                   # spatiotemporal = "rw",
                   silent = TRUE,
                   time = "year",
                   family = lognormal(link = "log"),
                   control = sdmTMBcontrol(newton_loops = 1L),
                   priors = sdmTMBpriors(
                     matern_s = pc_matern(range_gt = 25, sigma_lt = 2),
                     matern_st = pc_matern(range_gt = 25, sigma_lt = 2)))
  sanity(fit_tv)
  AIC(fit_tv)

  nd <- expand.grid(log_depth = seq(min(ds$log_depth),
                                    max(ds$log_depth),
                                    length.out = 50),
                    year = unique(ds$year)) # all years

  p <- predict(fit_tv, newdata = nd, se_fit = TRUE, re_form = NA)

  # add annual mean temperature for colouring the year lines
  td <- td |>
    group_by(fishing_event_id) |>
    summarise(temp = mean(avg, na.rm = TRUE))

  td <- left_join(ds, td) |>
    filter(depth_m < 200 & depth_m > 100) |>
    group_by(year) |>
    summarise(yr_mean_temp = mean(temp, na.rm = TRUE))

  p2 <- left_join(p, td) |>
    filter(log_depth < log(450)) |>
    filter(year != 2020) # temp data missing for 2020

  # check pattern within years of certain surveys
  # yrs <- ds %>% filter(survey_abbrev == "SYN WCVI")
  # p2 <- p2 %>% filter(year %in% unique(yrs$year))

  leg_text <- ifelse(fr(),
                     paste0("Température moyenne\n",
                            "au fond à des\n",
                            "profondeurs\n",
                            "comprises entre\n",
                            "100 et 200 m"),
                     paste0("Mean bottom\n",
                            "temperature at \n",
                            "100-200 m\n",
                            "depths"))

  g <- ggplot(p2,
              aes(log_depth,
                  exp(est),
                  ymin = exp(est - 1.96 * est_se),
                  ymax = exp(est + 1.96 * est_se),
                  group = as.factor(year))) +
    geom_line(aes(colour = yr_mean_temp), lwd = 1) +
    geom_ribbon(aes(fill = yr_mean_temp), alpha = 0.05) +
    scale_colour_viridis_c(option = "plasma") +
    scale_fill_viridis_c(option = "plasma") +
    scale_x_continuous(trans = "exp",
                       breaks = c(log(100),
                                  log(200),
                                  log(300),
                                  log(400),
                                  log(500)),
                       labels = c("100",
                                  "200",
                                  "300",
                                  "400",
                                  "500")) +
    coord_cartesian(expand = F) +
    labs(x = ifelse(fr(),
                    "Profondeur (m)",
                    "Depth (m)"),
         y = ifelse(fr(),
                    "Facteur de condition prédit",
                    "Predicted condition factor"),
         fill = leg_text,
         colour = leg_text) +
    theme_pbs()

  g
}