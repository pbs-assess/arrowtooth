library(ggplot2)
library(dplyr)
library(sdmTMB)
library(here)
source(here("geostat/utils.R"))

dir.create(here("arrowtooth-nongit/geostat-figs"), showWarnings = FALSE)
d <- readRDS(here("arrowtooth-nongit", "data", "arrowtooth-flounder-aug10-2021.rds"))

dat <- d$survey_samples
dset <- d$survey_sets

dat <- left_join(select(dat, fishing_event_id, year, length, sex, age, weight, usability_code, specimen_id), dset)
dat <- filter(dat, survey_abbrev %in% c("SYN HS"   ,"SYN QCS",  "SYN WCHG", "SYN WCVI"))
dat <- filter(dat, !is.na(depth_m))

mf <- gfplot::fit_length_weight(dat, sex = "female")
mm <- gfplot::fit_length_weight(dat, sex = "male")

df <- dplyr::filter(dat, sex == 2, !is.na(weight), !is.na(length))
dm <- dplyr::filter(dat, sex == 1, !is.na(weight), !is.na(length))

df$wbar <- exp(mf$pars$log_a) * df$length^mf$pars$b * 1000
dm$wbar <- exp(mm$pars$log_a) * dm$length^mm$pars$b * 1000

dd <- bind_rows(df, dm)
dd$cond_fac <- dd$weight / dd$wbar

dd <- filter(dd, cond_fac < quantile(dd$cond_fac, probs = 0.995))
dd <- filter(dd, cond_fac > quantile(dd$cond_fac, probs = 0.005))

group_by(dd, year) %>%
  summarise(mcond = mean(cond_fac)) %>%
  ggplot(aes(year, mcond)) + geom_line()

ds <- dd
ggplot(ds, aes(longitude, latitude, colour = log(cond_fac))) +
  geom_point() +
  facet_wrap(~year) +
  scale_colour_gradient2()

ds$X <- NULL
ds$Y <- NULL
ds <- sdmTMB::add_utm_columns(ds, ll_names = c("longitude", "latitude"), utm_crs = 32609)

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

# v <- visreg::visreg(fit, xvar = "log_depth")
# ggplot(v$res, aes(log_depth, cond_fac)) + geom_line()

png(here("arrowtooth-nongit/geostat-figs/condition-smoother.png"), width = 6, height = 4, units = "in", res = 200)
plot_smooth(fit, select = 1)
dev.off()

nd <- readRDS(here("geostat/synoptic_grid.rds"))
fitted_yrs <- sort(unique(ds$year))
nd <- make_grid(nd, years = fitted_yrs)
nd <- na.omit(nd)
nd$year <- as.integer(nd$year)
nd$log_depth <- log(nd$depth)

p <- predict(fit, newdata = nd, nsim = 300)
ind <- get_index_sims(p,
  # agg_function = function(x) median(x),
  agg_function = function(x) mean(x),
  area_function = function(x, area) x * area)

ggplot(ind, aes(year, exp(est), ymin = exp(lwr), ymax = exp(upr))) +
  geom_ribbon(alpha= 0.5) + geom_line() +
  gfplot::theme_pbs() +
  ylab("Condition factor") + xlab("Year")

ggsave(here("arrowtooth-nongit/geostat-figs/condition-coastwide.png"), width = 6, height = 4)

out <- split(nd, nd$survey) %>%
  purrr::map(function(.x) {
    p <- predict(fit, newdata = .x, nsim = 300)
    ind <- get_index_sims(p,
      agg_function = function(x) mean(x),
      area_function = function(x, area) x * area)
    ind
  })
outs <- bind_rows(out, .id = "survey")

ggplot(outs, aes(year, exp(est), ymin = exp(lwr), ymax = exp(upr), colour = survey, fill = survey)) +
  geom_ribbon(alpha= 0.5, colour = NA) + geom_line() +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  gfplot::theme_pbs() +
  ylab("Condition factor") + xlab("Year") +
  labs(fill = "Survey", colour = "Survey")
  # facet_wrap(~survey)
ggsave(here("arrowtooth-nongit/geostat-figs/condition-by-survey.png"), width = 6, height = 4)

pmap <- predict(fit, newdata = nd)

g <- ggplot(pmap, aes(X, Y, fill = est)) +
  geom_tile(width = 2, height = 2) +
  facet_wrap(~year) +
  scale_fill_gradient2() +
  gfplot::theme_pbs() +
  coord_fixed() +
  labs(fill = "Condition anomaly\n(log space)")

ggsave(here("arrowtooth-nongit/geostat-figs/condition-map.png"), width = 10, height = 10)
