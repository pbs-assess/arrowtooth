library(ggplot2)
library(dplyr)
library(sdmTMB)
library(here)
library(scales)

source(here("geostat/utils.R"))

dr <- paste0(here(), "-nongit")

dir.create(file.path(dr, "geostat-figs"), showWarnings = FALSE)
d <- readRDS(file.path(dr, "data", "arrowtooth-flounder-aug11-2022.rds"))

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
# ggplot(v$res, aes(log_depth, visregRes)) + geom_point()

png(file.path(dr, "geostat-figs", "condition-smoother.png"), width = 6, height = 4, units = "in", res = 200)

plot_smooth(fit, select = 1, ggplot = TRUE, rug = FALSE) +
  scale_x_continuous(trans = "exp",
                     breaks = c(log(100), log(200), log(300), log(400), log(500)),
                     labels = c("100", "200", "300", "400", "500")) +
  scale_y_continuous(breaks = c(0.97, 1, 1.03, 1.06, 1.09)) +
  coord_cartesian(expand = FALSE) +
  ylab("Predicted condition factor") +
  xlab("Depth (m)") +
  ggplot2::geom_rug(
    data = fit$data, mapping = ggplot2::aes_string(x = "log_depth"),
    sides = "t", inherit.aes = FALSE, alpha = 0.01, size = 0.5
  ) +
  gfplot::theme_pbs()

dev.off()



## time-varying depth effect on condition

fit_tv <- sdmTMB(cond_fac ~ 0 + s(year),
              time_varying = ~ 0 + poly(log_depth, 3, raw = T),
              mesh = mesh,
              data = ds,
              # spatial = "off",
              spatial = "on",
              # spatiotemporal = "rw",
              silent = FALSE,
              time = "year",
              family = lognormal(link = "log"),
              control = sdmTMBcontrol(newton_loops = 1L),
              priors = sdmTMBpriors(
                matern_s = pc_matern(range_gt = 25, sigma_lt = 2),
                matern_st = pc_matern(range_gt = 25, sigma_lt = 2)
              ))
sanity(fit_tv)

AIC(fit_tv)


nd <- expand.grid(
  log_depth = seq(min(ds$log_depth),
                     max(ds$log_depth),
                     length.out = 50
  ),
  year = unique(ds$year) # all years
)

p <- predict(fit_tv, newdata = nd, se_fit = TRUE, re_form = NA)

# add annual mean temperature for colouring the year lines
td <- readRDS(file.path(dr, "data", "trawl_temp.rds")) %>% group_by(fishing_event_id) %>% summarise(temp = mean(avg, na.rm =T))
td <- left_join(ds, td) %>% filter(depth_m < 200 & depth_m > 100) %>% group_by(year) %>% summarise(yr_mean_temp = mean(temp, na.rm = T))
p2 <- left_join(p, td) %>%
  filter(log_depth < log(450)) %>%
  filter(year != 2020) # temp data missing for 2020

## check pattern within years of certain surveys
# yrs <- ds %>% filter(survey_abbrev == "SYN WCVI")
# p2 <- p2 %>% filter(year %in% unique(yrs$year))

png(file.path(dr, "geostat-figs", "condition-tv-raw-poly-3.png"), width = 6, height = 4, units = "in", res = 200)

ggplot(p2, aes(log_depth, exp(est),
              ymin = exp(est - 1.96 * est_se),
              ymax = exp(est + 1.96 * est_se),
              group = as.factor(year)
)) +
  geom_line(aes(colour = yr_mean_temp), lwd = 1) +
  geom_ribbon(aes(fill = yr_mean_temp), alpha = 0.05) +
  scale_colour_viridis_c(option = "plasma") +
  scale_fill_viridis_c(option = "plasma") +
  scale_x_continuous(trans = "exp",
                     breaks = c(log(100), log(200), log(300), log(400), log(500)),
                     labels = c("100", "200", "300", "400", "500")) +
  coord_cartesian(expand = F) +
  labs(x = "Depth (m)", y = "Predicted condition factor",
       fill = "Mean bottom \ntemperature at \n100-200 m \ndepths",
       colour = "Mean bottom \ntemperature at \n100-200 m \ndepths")+
  gfplot::theme_pbs()

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

ggsave(file.path(dr, "geostat-figs", "condition-coastwide.png"), width = 6, height = 4)

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
ggsave(file.path(dr, "geostat-figs", "condition-by-survey.png"), width = 6, height = 4)

pmap <- predict(fit, newdata = nd)

g <- ggplot(pmap, aes(X, Y, fill = est)) +
  geom_tile(width = 2, height = 2) +
  facet_wrap(~year) +
  scale_fill_gradient2() +
  gfplot::theme_pbs() +
  coord_fixed() +
  labs(fill = "Condition anomaly\n(log space)")

ggsave(file.path(dr, "geostat-figs", "condition-map.png"), width = 10, height = 10)
