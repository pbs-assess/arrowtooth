# do <- readRDS(here("geostat/bc-trawl-env.rds"))
# dg <- readRDS(here("geostat/bc-trawl-grid-env.rds"))

library(dplyr)
library(ggplot2)
library(here)
library(sdmTMB)

library(gfiscamutils)

source(here("geostat/utils.R"))

dr <- "/srv/arrowtooth/arrowtooth-nongit"
#dr <- paste0(here(), "-nongit")

dat <- prep_data(folder = dr)
list_species <- "arrowtooth flounder"

f <- file.path(dr, "figures-geostat")
if (!file.exists(file.path(f, "geo-delta-gamma-depth.rds"))) { # pick one
  out <- purrr::map(list_species, ~
      fit_index(
        dat,
        folder = dr,
        species = .x
      )
  ) %>% setNames(list_species)

  out_nodepth <- purrr::map(list_species, ~
      fit_index(
        dat,
        folder = dr,
        species = .x,
        formula = catch_weight ~ 1,
      )
  ) %>% setNames(list_species)

  out_dg_nodepth <- purrr::map(list_species, ~
      fit_index(
        dat,
        folder = dr,
        species = .x,
        formula = catch_weight ~ 1,
        family = delta_gamma()
      )
  ) %>% setNames(list_species)

  out_dg <- purrr::map(list_species, ~
      fit_index(
        dat,
        folder = dr,
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
ggsave(file.path(f, "geostat-indexes.png"), width = 7, height = 4)

# AIC(out$`arrowtooth flounder`$fit)
# AIC(out_nodepth$`arrowtooth flounder`$fit)
# AIC(out_dg$`arrowtooth flounder`$fit)
# AIC(out_dg_nodepth$`arrowtooth flounder`$fit)

ind <- index %>%
  bind_rows(index_nodepth) %>%
  bind_rows(index_dg) %>%
  bind_rows(index_dg_nodepth) %>%
  mutate(cv = sqrt(exp(se^2) - 1))
saveRDS(ind, file.path(f, "geostat-stitched-index.rds"))

group_by(ind, type) %>%
  summarise(mean_cv = mean(cv))

data_dir <- file.path("../arrowtooth-nongit/", "data")
discard_cpue_file <- file.path(data_dir,
  "cpue-predictions-arrowtooth-flounder-modern-3CD5ABCDE-discard-july-26-feb-fishing-year.csv")
discard_cpue <- readr::read_csv(discard_cpue_file)
discard_cpue <- dplyr::filter(discard_cpue, formula_version == "Full standardization")

ind_cent <- ind %>% group_by(type) %>%
  mutate(geo_mean = exp(mean(log_est))) %>%
  mutate(
    est = est / geo_mean,
    lwr = lwr / geo_mean,
    upr = upr / geo_mean,
    type = paste0("Survey ", type),
    type2 = "a-survey"
  )

cpue_cent <- discard_cpue %>%
  mutate(geo_mean = exp(mean(est_link[year %in% 2003:2021]))) %>%
  mutate(
    est = est / geo_mean,
    lwr = lwr / geo_mean,
    upr = upr / geo_mean,
    type = "Discard CPUE",
    type2 = "b-cpue"
  )

comb <- cpue_cent %>% bind_rows(ind_cent)
levels(comb$type) <- c("Discard CPUE", "Survey Tweedie s(depth)", "Survey Tweedie",
  "Survey Delta-Gamma s(depth)", "Survey Delta-Gamma")

cols <- c("grey30", RColorBrewer::brewer.pal(4, "Set2"))

comb %>%
  ggplot(aes(year, est, colour = type, fill = type,
    ymin = lwr, ymax = upr, lty = type2)) +
  gfplot::theme_pbs() +
  geom_ribbon(alpha = 0.3, colour = NA) +
  geom_line() +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  labs(fill = "Type", colour = "Type") +
  ylab("Scaled index") + xlab("Year") +
  guides(lty = "none")

ggsave(file.path(f, "geostat-indexes-cpue.png"), width = 7, height = 4)

# simulate(out$`arrowtooth flounder`$fit, 200) %>%
#   dharma_residuals(out$`arrowtooth flounder`$fit)
#
# simulate(out_dg_nodepth$`arrowtooth flounder`$fit, 200) %>%
#   dharma_residuals(out$`arrowtooth flounder`$fit)
#
# simulate(out_dg_nodepth$`arrowtooth flounder`$fit, 200) %>%
#   dharma_residuals(out$`arrowtooth flounder`$fit)

# r0 <- residuals(out$`arrowtooth flounder`$fit, "mle-mcmc",
#   model = 1L, mcmc_iter = 101, mcmc_warmup = 100)
#
# r1 <- residuals(out_dg$`arrowtooth flounder`$fit, "mle-mcmc",
#   model = 1L, mcmc_iter = 101, mcmc_warmup = 100)
#
# r2 <- residuals(out_dg$`arrowtooth flounder`$fit, "mle-mcmc",
#   model = 2L, mcmc_iter = 101, mcmc_warmup = 100)

m <- out_dg_nodepth$`arrowtooth flounder`$fit
nd <- readRDS(file.path(f, "synoptic_grid.rds"))
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
ggsave(file.path(f, "geostat-map-pred1.png"), width = 9, height = 11)

g <- pp %>% filter(year > 2012) %>%
  plot_map("est_total", max_colour = max(pp$est_total))
ggsave(file.path(f, "geostat-map-pred2.png"), width = 9, height = 10)

# .f <- file.path(f, "mcmc-resids.rda")
# if (!file.exists(.f)) {
#   r0 <- residuals(out$`arrowtooth flounder`$fit, "mle-mcmc",
#     model = 1L, mcmc_iter = 101, mcmc_warmup = 100)
#   r1 <- residuals(out_dg$`arrowtooth flounder`$fit, "mle-mcmc",
#     model = 1L, mcmc_iter = 101, mcmc_warmup = 100)
#   r2 <- residuals(out_dg$`arrowtooth flounder`$fit, "mle-mcmc",
#     model = 2L, mcmc_iter = 101, mcmc_warmup = 100)
#   save(r0, r1, r2, file = file.path(f, "mcmc-resids.rda"))
# } else {
#   load(.f)
# }
#
# png(file.path(f, "qq-mcmc-coast.png"), width = 7, height = 7, units = "in", res = 200)
# par(mfrow = c(2, 2), cex = 0.8)
# qqnorm(r1, main = "Binomial");qqline(r1)
# qqnorm(r2, main = "Gamma");qqline(r2)
# qqnorm(r0, main = "Tweedie");qqline(r0)
# dev.off()

group_by(ind, type) %>%
  summarise(mean_cv = mean(cv))

dd <- readRDS(file.path(dr, "data", "arrowtooth-flounder-aug11-2022.rds"))$survey_index
dd <- dd %>% filter(survey_abbrev %in%  c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"))
.files <- list.files(file.path(dr, "survey-geostat"), full.names = TRUE, pattern = "^i-arrow")
ind_geo <- purrr::map_dfr(.files, readRDS)

for_model <- ind_geo %>% filter(model == "with-depth")
for_model <- for_model |> mutate(cv = sqrt(exp(se^2) - 1)) %>%
  select(year, survey, est, lwr, upr, se, log_est, cv) |>
  mutate(weight = 1/cv) |>
  mutate(bio = round(est / 1000000, 2), weight = round(weight, 2)) |>
  select(survey, year, bio, weight)
# for_model |> readr::write_("geostat.csv")

g <- bind_rows(ind_geo %>% filter(model == "with-depth") %>%
    mutate(model = "Geostatistical (with depth)"),
  select(dd, year, lwr = lowerci, upr = upperci, est = biomass, survey = survey_abbrev) %>%
    mutate(model = "Design-based")) %>%
  ggplot(aes(year, est / 1000, colour = model, fill = model))
g <- g +
  geom_ribbon(aes(ymin = lwr / 1000, ymax = upr / 1000),
    alpha = 0.2, colour = NA) +
  geom_line(alpha = 1, lty = 1, lwd = 0.5)
g <- g +
  # geom_pointrange(aes(ymin = lwr / 1000, ymax = upr / 1000), position = position_dodge(width = 0))
  geom_point(pch = 21, fill = NA)
g <- g + ylab("Biomass (tonnes)") +
  coord_cartesian(
    # ylim = c(0, max(ind$upr / 1000) * 1.03),
    expand = FALSE, xlim = range(ind$year) + c(-0.25, 0.25)
  ) +
  gfplot::theme_pbs() +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~survey, scales = "free_y") +
  labs(fill = "Type", colour = "Type") +
  scale_fill_brewer(palette = "Set2") +
  scale_colour_brewer(palette = "Set2")
g
ggsave(file.path(f, "geostat-individual-vs-design.png"), width = 7, height = 4)


# s <- c(1, 3, 4, 16)
# out <- lapply(s, function(.s) {
#   cat(.s, "\n")
#   m <- readRDS(paste0("~/src/gfindex/models/m-arrowtooth-flounder-", .s, "-no-depth.rds"))
#   r1 <- residuals(m, type = "mle-mcmc", mcmc_iter = 101, mcmc_warmup = 100, model = 1)
#   r2 <- residuals(m, type = "mle-mcmc", mcmc_iter = 101, mcmc_warmup = 100, model = 2)
#   data.frame(r1 = r1, r2 = r2)
# })



# ggsave("")

# g <- ggplot(ind, aes(year, est / 1000)) +
#   ggsidekick::theme_sleek()
# g <- g +
#   geom_ribbon(aes(ymin = lwr / 1000, ymax = upr / 1000),
#     alpha = 0.9, fill = "grey90"
#   ) +
#   geom_line(alpha = 0.9, lty = 1, lwd = 0.5)
# g
