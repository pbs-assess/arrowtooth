library(dplyr)
library(ggplot2)
library(here)
library(sdmTMB)

source(here("geostat/utils.R"))

dat <- prep_data()
list_species <- "arrowtooth flounder"

if (!file.exists("~/src/arrowtooth/arrowtooth-nongit/geo-tweedie-depth")) {
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
  saveRDS(out, file = "~/src/arrowtooth/arrowtooth-nongit/geo-tweedie-depth")
  saveRDS(out_nodepth, file = "~/src/arrowtooth/arrowtooth-nongit/geo-tweedie-nodepth")
  saveRDS(out_dg, file = "~/src/arrowtooth/arrowtooth-nongit/geo-delta-gamma-depth")
  saveRDS(out_dg_nodepth, file = "~/src/arrowtooth/arrowtooth-nongit/geo-delta-gamma-nodepth")
} else {
  out <- readRDS("~/src/arrowtooth/arrowtooth-nongit/geo-tweedie-depth")
  out_nodepth <- readRDS("~/src/arrowtooth/arrowtooth-nongit/geo-tweedie-nodepth")
  out_dg <- readRDS("~/src/arrowtooth/arrowtooth-nongit/geo-delta-gamma-depth")
  out_dg_nodepth <- readRDS("~/src/arrowtooth/arrowtooth-nongit/geo-delta-gamma-nodepth")
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

# AIC(out$`arrowtooth flounder`$fit)
# AIC(out_nodepth$`arrowtooth flounder`$fit)
# AIC(out_dg$`arrowtooth flounder`$fit)
# AIC(out_dg_nodepth$`arrowtooth flounder`$fit)

ind <- index %>%
  bind_rows(index_nodepth) %>%
  bind_rows(index_dg) %>%
  bind_rows(index_dg_nodepth)

group_by(ind, type) %>%
  summarise(mean_se = mean(se))

simulate(out$`arrowtooth flounder`$fit, 200) %>%
  dharma_residuals(out$`arrowtooth flounder`$fit)

simulate(out_dg$`arrowtooth flounder`$fit, 200) %>%
  dharma_residuals(out$`arrowtooth flounder`$fit)

simulate(out_dg_nodepth$`arrowtooth flounder`$fit, 200) %>%
  dharma_residuals(out$`arrowtooth flounder`$fit)

r0 <- residuals(out$`arrowtooth flounder`$fit, "mle-mcmc",
  model = 1L, mcmc_iter = 101, mcmc_warmup = 100)

r1 <- residuals(out_dg$`arrowtooth flounder`$fit, "mle-mcmc",
  model = 1L, mcmc_iter = 101, mcmc_warmup = 100)

r2 <- residuals(out_dg$`arrowtooth flounder`$fit, "mle-mcmc",
  model = 2L, mcmc_iter = 101, mcmc_warmup = 100)

saveRDS(out, file = "~/src/arrowtooth/arrowtooth-nongit/geo-tweedie-depth")
saveRDS(out_nodepth, file = "~/src/arrowtooth/arrowtooth-nongit/geo-tweedie-nodepth")
saveRDS(out_dg, file = "~/src/arrowtooth/arrowtooth-nongit/geo-delta-gamma-depth")
saveRDS(out_dg_nodepth, file = "~/src/arrowtooth/arrowtooth-nongit/geo-delta-gamma-nodepth")

m <- out_dg$`arrowtooth flounder`$fit
nd <- readRDS(here("grids/synoptic_grid.rds"))
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
    geom_point(alpha = 0.8, pch = 21) +
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

dat %>%
  # filter(year %in% 2005:2007) %>%
  arrange(-density) %>%
  plot_multiyear_survey_sets()
# data <- dplyr::filter(data, !(year == 2014 & survey_abbrev == "SYN WCHG")) # not used

plot_map <- function(dat, column) {
  ggplot(dat, aes_string("X", "Y", fill = column, colour = column)) +
    geom_polygon(
      data = coast, aes_string(x = "X", y = "Y", group = "PID"),
      fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE
    ) +
    geom_tile(width = 2, height = 2) +
    scale_colour_viridis_c(trans = "sqrt") +
    scale_fill_viridis_c(trans = "sqrt") +
    coord_fixed() +
    coords +
    utm_labs
}

p %>%
  mutate(est_total = plogis(est1) * exp(est2)) %>%
  mutate(est_total = ifelse(est_total > quantile(est_total, probs = 0.995), quantile(est_total, probs = 0.995), est_total)) %>%
  # filter(year %in% 2007:2009) %>%
  plot_map("est_total") +
  facet_wrap(vars(year)) +
  labs(fill = "Biomass density", colour = "Biomass density") +
  gfplot::theme_pbs()

# ggsave("")

# g <- ggplot(ind, aes(year, est / 1000)) +
#   ggsidekick::theme_sleek()
# g <- g +
#   geom_ribbon(aes(ymin = lwr / 1000, ymax = upr / 1000),
#     alpha = 0.9, fill = "grey90"
#   ) +
#   geom_line(alpha = 0.9, lty = 1, lwd = 0.5)
# g
