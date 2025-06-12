#' Fit a Geo statistical spatial model
#'
#' @param dat A data frame containing `survey_sets` as returned by
#' [gfdata::get_survey_sets()]
#' @param species The species common name
#' @param grid_fn Filename for the grid data (RDS file)
#' @param formula The model formula
#' @param family Family of distribution to use for the model fit
#' @param cutoff The minimum allowable triangle edge length. See
#' [sdmTMB::make_mesh()]
#' @param anisotropy Logical. If `TRUE`, allow for spatial correlation that
#' is directionally dependent. See [sdmTMB::sdmTMB()]
#' @param spatiotemporal Method used to estimate the spatiotemporal
#' random fields. Can be 'iid', 'ar1', or 'rw'. See [sdmTMB::sdmTMB()]
#' @param silent Silent or include optimization details? Helpful to set
#' to `FALSE` for models that take a while to fit. See [sdmTMB::sdmTMB()]
#' @param priors Optional penalties/priors via [sdmTMB::sdmTMBpriors()],
#' See [sdmTMB::sdmTMB()]
#' @param ... Arguments passed to [sdmTMB::sdmTMB()]
#'
#' @return A list of three elements: sanity, fit and index
#' @export
geostat_fit_index <- \(
  dat,
  species = "arrowtooth flounder",
  grid_fn = "/srv/arrowtooth/arrowtooth-nongit/data/synoptic_grid.rds",
  formula = catch_weight ~ s(log_depth, k = 5),
  family = tweedie(),
  cutoff = 20,
  bias_correct = TRUE,
  anisotropy = FALSE,
  spatiotemporal = "rw",
  silent = FALSE,
  priors = sdmTMBpriors(
    matern_s = pc_matern(range_gt = 10, sigma_lt = 4),
    matern_st = pc_matern(range_gt = 10, sigma_lt = 3)),
  ...) {

  if(!file.exists(grid_fn)){
    bail("File `", grid_fn, "` does not exist.")
  }

  dat <- dat |>
    filter(species_common_name == tolower(species))

  region <- unique(dat$survey_abbrev)

  ctrl <- sdmTMBcontrol(newton_loops = 1L)
  dat <- dat |>
    filter(!is.na(depth_m))

  mesh <- make_mesh(dat, c("X", "Y"), cutoff = 20)
  # plot(mesh$mesh, asp = 1)
  # points(dat$X, dat$Y, pch = ".")
  # mesh$mesh$n

  nd <- readRDS(grid_fn) |>
    filter(survey %in% region)
  fitted_yrs <- sort(unique(dat$year))
  nd <- make_grid(nd, years = fitted_yrs)
  nd <- na.omit(nd)
  nd$year <- as.integer(nd$year)
  nd$log_depth <- log(nd$depth)
  nd$cell_area <- 4e+6

  openmp(n = 2L, DLL = "sdmTMB")
  fit <- try(
    sdmTMB(
      formula,
      dat,
      mesh = mesh,
      time = "year",
      family = family,
      spatial = "on",
      spatiotemporal = spatiotemporal,
      offset = log(dat$area_swept / 1e5),
      share_range = TRUE,
      anisotropy = anisotropy,
      silent = silent,
      control = ctrl,
      do_index = TRUE,
      priors = priors,
      index_args = list(area = nd$cell_area / 1e5),
      predict_args = list(newdata = nd, return_tmb_object = TRUE),
      ...))
  s <- sanity(fit)

  if(!inherits(s, "logical")){
    if(s$all_ok && class(fit) != "try-error"){
      if (bias_correct)
        openmp(n = 1L, DLL = "sdmTMB")
      ind <- get_index(fit,
                       bias_correct = bias_correct,
                       area = nd$cell_area / 1e5)
    }else{
      ind <- NA
    }
  }else{
    ind <- NA
  }

  list(sanity = s,
       fit = fit,
       index = ind)
}
