fit_index <- function(dat,
  species = "arrowtooth flounder",
  folder = ".",
  formula = catch_weight ~ s(log_depth, k = 5),
  family = tweedie(),
  cutoff = 20,
  bias_correct = TRUE,
  anisotropy = FALSE,
  spatiotemporal = "rw",
  silent = FALSE,
  priors = sdmTMBpriors(
    matern_s = pc_matern(range_gt = 10, sigma_lt = 4),
    matern_st = pc_matern(range_gt = 10, sigma_lt = 3)
  ),
  ...) {
  dat <- dplyr::filter(dat, species_common_name == tolower(species))
  region <- unique(dat$survey_abbrev)

  ctrl <- sdmTMBcontrol(newton_loops = 1L)
  dat <- dplyr::filter(dat, !is.na(depth_m))

  mesh <- make_mesh(dat, c("X", "Y"), cutoff = 20)
  # plot(mesh$mesh, asp = 1)
  # points(dat$X, dat$Y, pch = ".")
  # mesh$mesh$n

  nd <- readRDS(file.path(folder, "figures-geostat", "synoptic_grid.rds")) %>%
    dplyr::filter(survey %in% region)
  fitted_yrs <- sort(unique(dat$year))
  nd <- make_grid(nd, years = fitted_yrs)
  nd <- na.omit(nd)
  nd$year <- as.integer(nd$year)
  nd$log_depth <- log(nd$depth)
  nd$cell_area <- 4e+6

  TMB::openmp(n = 2L, DLL = "sdmTMB")
  fit <- try(
    sdmTMB(
      formula,
      dat,
      mesh = mesh,
      time = "year",
      family = family,
      spatial = "on",
      spatiotemporal = spatiotemporal,
      offset = log(dat$area_swept / 100000),
      share_range = TRUE,
      anisotropy = anisotropy,
      silent = silent,
      control = ctrl,
      do_index = TRUE,
      priors = priors,
      index_args = list(area = nd$cell_area / 100000),
      predict_args = list(newdata = nd, return_tmb_object = TRUE),
      ...
    )
  )
  s <- sanity(fit)
  if (s$all_ok && class(fit) != "try-error") {
    if (bias_correct)
    TMB::openmp(n = 1L, DLL = "sdmTMB")
    ind <- get_index(fit, bias_correct = bias_correct, area = nd$cell_area / 100000)
  } else {
    ind <- NA
  }

  list(sanity = s, fit = fit, index = ind)
}

make_grid <- function(.x, years) {
  years <- sort(unique(years))
  .nd <- do.call(
    "rbind",
    replicate(length(years), .x, simplify = FALSE)
  )
  .nd$year <- rep(years, each = nrow(.x))
  .nd
}
