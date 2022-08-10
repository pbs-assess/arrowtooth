# do <- readRDS(here("geostat/bc-trawl-env.rds"))
# dg <- readRDS(here("geostat/bc-trawl-grid-env.rds"))

library(dplyr)
library(ggplot2)
library(here)
library(sdmTMB)

source(here("geostat/utils.R"))

dat <- prep_data()
list_species <- "arrowtooth flounder"

f <- here("arrowtooth-nongit/geostat-figs/")
if (!file.exists(file.path(f, "geo-delta-gamma-depth.rds"))) { # pick one
  out <- purrr::map(list_species, ~
      fit_index(
        dat,
        species = .x
      )
  ) %>% setNames(list_species)

  out_nodepth <- purrr::map(list_species, ~
      fit_index(
        dat,
        species = .x,
        formula = catch_weight ~ 1,
      )
  ) %>% setNames(list_species)
  out_dg_nodepth <- purrr::map(list_species, ~
      fit_index(
        dat,
        species = .x,
        formula = catch_weight ~ 1,
        family = delta_gamma()
      )
  ) %>% setNames(list_species)

  out_dg <- purrr::map(list_species, ~
      fit_index(
        dat,
        species = .x,
        family = delta_gamma()
      )
  ) %>% setNames(list_species)
  saveRDS(out, file = file.path(f, "geo-tweedie-depth.rds"))
  saveRDS(out_nodepth, file = file.path(f, "geo-tweedie-nodepth.rds"))
  saveRDS(out_dg, file = file.path(f, "geo-delta-gamma-depth.rds"))
  saveRDS(out_dg_nodepth, file = file.path(f, "geo-delta-gamma-nodepth.rds"))
} else {
  out <- readRDS(file.path(f, "geo-tweedie-depth.rds"))
  out_nodepth <- readRDS(file.path(f, "geo-tweedie-nodepth.rds"))
  out_dg <- readRDS(file.path(f, "geo-delta-gamma-depth.rds"))
  out_dg_nodepth <- readRDS(file.path(f, "geo-delta-gamma-nodepth.rds"))
}

index_dg_nodepth <- purrr::map_dfr(out_dg_nodepth, "index", .id = "species")
index_nodepth <- purrr::map_dfr(out_nodepth, "index", .id = "species")
index <- purrr::map_dfr(out, "index", .id = "species")
index_dg <- purrr::map_dfr(out_dg, "index", .id = "species")

index <- index %>% mutate(type = "Tweedie s(depth)")
index_nodepth <- index_nodepth %>% mutate(type = "Tweedie")
index_dg <- index_dg %>% mutate(type = "Delta-Gamma s(depth)")
index_dg_nodepth <- index_dg_nodepth %>% mutate(type = "Delta-Gamma")

mult <- 1000

g <- index %>%
  bind_rows(index_nodepth) %>%
  bind_rows(index_dg) %>%
  bind_rows(index_dg_nodepth) %>%
  group_by(species, type) %>%
  mutate(includes_depth = grepl("depth", type)) %>%
  mutate(max_log = max(log_est)) %>%
  filter(max_log < 20) %>%
  mutate(species = stringr::str_to_title(species)) %>%
  ggplot(aes(year, est / mult, colour = type, fill = type,
    ymin = lwr / mult, ymax = upr / mult)) +
  gfplot::theme_pbs()
g <- g +
  # geom_pointrange(aes(ymin = lwr / mult, ymax = upr / mult))
  geom_ribbon(alpha = 0.25, colour = NA) +
  # geom_line(aes(lty = includes_depth), lwd = 0.8)
  geom_line(lwd = 0.8)
g <- g + ylab("Biomass (tonnes)") +
  # ggtitle(stringr::str_to_title(species), subtitle = paste(region, collapse = ", ")) +
  coord_cartesian(
    ylim = c(0, 70000),
    expand = FALSE, xlim = range(index$year) + c(-0.25, 0.25)
  ) +
  theme(axis.title.x = element_blank()) +
  labs(colour = "Model", fill = "Model", linetype = "Includes s(depth)") +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2") +
  scale_linetype_manual(values = c(2, 1))
g
ggsave(here("arrowtooth-nongit/geostat-figs/geostat-indexes.png"), width = 7, height = 4)

# AIC(out$`arrowtooth flounder`$fit)
# AIC(out_nodepth$`arrowtooth flounder`$fit)
# AIC(out_dg$`arrowtooth flounder`$fit)
# AIC(out_dg_nodepth$`arrowtooth flounder`$fit)

ind <- index %>%
  bind_rows(index_nodepth) %>%
  bind_rows(index_dg) %>%
  bind_rows(index_dg_nodepth) %>%
  mutate(cv = sqrt(exp(se^2) - 1))
saveRDS(ind, here("arrowtooth-nongit/geostat-figs/geostat-stitched-index.rds"))

group_by(ind, type) %>%
  summarise(mean_cv = mean(cv))

simulate(out$`arrowtooth flounder`$fit, 200) %>%
  dharma_residuals(out$`arrowtooth flounder`$fit)

simulate(out_dg_nodepth$`arrowtooth flounder`$fit, 200) %>%
  dharma_residuals(out$`arrowtooth flounder`$fit)

simulate(out_dg_nodepth$`arrowtooth flounder`$fit, 200) %>%
  dharma_residuals(out$`arrowtooth flounder`$fit)

# r0 <- residuals(out$`arrowtooth flounder`$fit, "mle-mcmc",
#   model = 1L, mcmc_iter = 101, mcmc_warmup = 100)
#
# r1 <- residuals(out_dg$`arrowtooth flounder`$fit, "mle-mcmc",
#   model = 1L, mcmc_iter = 101, mcmc_warmup = 100)
#
# r2 <- residuals(out_dg$`arrowtooth flounder`$fit, "mle-mcmc",
#   model = 2L, mcmc_iter = 101, mcmc_warmup = 100)

m <- out_dg_nodepth$`arrowtooth flounder`$fit
nd <- readRDS(here("arrowtooth-nongit/geostat-figs/synoptic_grid.rds"))
fitted_yrs <- sort(unique(dat$year))
nd <- make_grid(nd, years = fitted_yrs)
nd <- na.omit(nd)
nd$year <- as.integer(nd$year)
nd$log_depth <- log(nd$depth)
p <- predict(m, newdata = nd)

data <- dat
coast <- gfplot:::load_coastline(
  range(data$longitude) + c(-0.5, 0.5),
  range(data$latitude) + c(-0.5, 0.5),
  utm_zone = 9
)
coords <- coord_equal(
  expand = FALSE, xlim = range(data$X) + c(-10, 10),
  ylim = range(data$Y) + c(-10, 10)
)
utm_labs <- labs(x = "Easting", y = "Northing")

library(rosettafish)
plot_multiyear_survey_sets <- function(dat, survey_abbrev,
  density_lab = "", french = FALSE) {
  density_lab <- if (french) {
    bquote(atop("Densité de la biomasse", ~ (kg / km^2)))
  } else {
    bquote(atop("Biomass density", ~ (kg / km^2)))
  }
  ggplot(filter(dat, density > 0), aes(X, Y,
    size = density,
    fill = density,
    colour = density
  )) +
    scale_size_area(max_size = 8) +
    geom_point(
      data = filter(dat, density == 0), pch = 4,
      size = 0.7, col = "grey60"
    ) +
    coords +
    geom_point(alpha = 0.5, pch = 21) +
    facet_wrap(~year) +
    geom_polygon(
      data = coast, aes_string(x = "X", y = "Y", group = "PID"),
      fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE
    ) +
    scale_fill_viridis_c(trans = "sqrt") +
    scale_colour_viridis_c(trans = "sqrt") +
    labs(
      fill = density_lab,
      colour = density_lab,
      size = density_lab, x = en2fr("Easting", translate = french),
      y = en2fr("Northing", translate = french)
    ) +
    guides(
      size = guide_legend(order = 1),
      fill = guide_colorbar(order = 0),
      colour = "none"
    ) +
    gfplot::theme_pbs()
}

g <- dat %>%
  filter(year %in% 2003:2012) %>%
  arrange(-density) %>%
  plot_multiyear_survey_sets() +
  facet_wrap(~year, ncol = 3)
# data <- dplyr::filter(data, !(year == 2014 & survey_abbrev == "SYN WCHG")) # not used
ggsave(here("arrowtooth-nongit/geostat-figs/geostat-map-raw1.png"), width = 9, height = 11)

g <- dat %>%
  filter(year > 2012) %>%
  arrange(-density) %>%
  plot_multiyear_survey_sets() +
  facet_wrap(~year, ncol = 3)
ggsave(here("arrowtooth-nongit/geostat-figs/geostat-map-raw2.png"), width = 9, height = 10)

plot_map <- function(dat, column, max_colour) {
  ggplot(dat, aes_string("X", "Y", fill = column, colour = column)) +
    geom_polygon(
      data = coast, aes_string(x = "X", y = "Y", group = "PID"),
      fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE
    ) +
    geom_tile(width = 2, height = 2) +
    scale_colour_viridis_c(trans = "sqrt", limits = c(0, max_colour)) +
    scale_fill_viridis_c(trans = "sqrt", limits = c(0, max_colour)) +
    coord_fixed() +
    coords +
    utm_labs +
    facet_wrap(vars(year), ncol = 3) +
  labs(fill = "Biomass density", colour = "Biomass density") +
    gfplot::theme_pbs()
}

pp <- p %>%
  mutate(est_total = plogis(est1) * exp(est2)) %>%
  mutate(est_total = ifelse(est_total > quantile(est_total, probs = 0.995), quantile(est_total, probs = 0.995), est_total))

g <- pp %>% filter(year <= 2012) %>%
  plot_map("est_total", max_colour = max(pp$est_total))
ggsave(here("arrowtooth-nongit/geostat-figs/geostat-map-pred1.png"), width = 9, height = 11)

g <- pp %>% filter(year > 2012) %>%
  plot_map("est_total", max_colour = max(pp$est_total))
ggsave(here("arrowtooth-nongit/geostat-figs/geostat-map-pred2.png"), width = 9, height = 10)

# ggsave("")

# g <- ggplot(ind, aes(year, est / 1000)) +
#   ggsidekick::theme_sleek()
# g <- g +
#   geom_ribbon(aes(ymin = lwr / 1000, ymax = upr / 1000),
#     alpha = 0.9, fill = "grey90"
#   ) +
#   geom_line(alpha = 0.9, lty = 1, lwd = 0.5)
# g
