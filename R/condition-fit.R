#' Run fit to condition model and return the result
#'
#' @param d A data frame, as returned by [condition_data_prep()]
#'
#' @returns A list of two items, `fit` which is a fit from [sdmTMB] and `ds`
#' which is a data frame of the data used in that fit
#' @export
condition_fit <- \(d){

  d <- d |>
    filter(survey_abbrev %in% c("SYN HS",
                                "SYN QCS",
                                "SYN WCHG",
                                "SYN WCVI")) |>
    filter(!is.na(depth_m))

  mf <- d |>
    fit_length_weight(sex = "female")
  mm <- d |>
    fit_length_weight(sex = "male")

  df <- d |>
    filter(sex == 2,
           !is.na(weight),
           !is.na(length)) |>
    mutate(wbar = exp(mf$pars$log_a) * length ^ mf$pars$b * 1e3)

  dm <- d |>
    filter(sex == 1,
           !is.na(weight),
           !is.na(length)) |>
    mutate(wbar = exp(mm$pars$log_a) * length ^ mm$pars$b * 1e3)

  dd <- bind_rows(df, dm) |>
    mutate(cond_fac = weight / wbar) |>
    filter(cond_fac < quantile(cond_fac, probs = 0.995)) |>
    filter(cond_fac > quantile(cond_fac, probs = 0.005))

  # group_by(dd, year) %>%
  #   summarise(mcond = mean(cond_fac)) %>%
  #   ggplot(aes(year, mcond)) + geom_line()

  ds <- dd
  ds$X <- NULL
  ds$Y <- NULL
  ds <- sdmTMB::add_utm_columns(ds,
                                ll_names = c("longitude", "latitude"),
                                utm_crs = 32609)

  # ggplot(ds, aes(longitude, latitude, colour = log(cond_fac))) +
  #   geom_point() +
  #   facet_wrap(~year) +
  #   scale_colour_gradient2()
  mesh <- make_mesh(ds, c("X", "Y"), cutoff = 20)
  # plot(mesh)

  ds$log_depth <- log(ds$depth_m)
  fit <- sdmTMB(cond_fac ~ 1 + s(log_depth),
                mesh = mesh,
                data = ds,
                spatial = "off",
                spatiotemporal = "rw",
                silent = FALSE,
                time = "year",
                family = lognormal(link = "log"),
                control = sdmTMBcontrol(newton_loops = 1L),
                priors = sdmTMBpriors(
                  matern_s = pc_matern(range_gt = 25, sigma_lt = 2),
                  matern_st = pc_matern(range_gt = 25, sigma_lt = 2)
                ))
  sanity(fit)

  list(fit = fit,
       ds = ds)
}