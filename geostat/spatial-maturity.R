# spatial maturity
library(sdmTMB)
glimpse(dat$survey_samples)
glimpse(dat$survey_sets)

d <- left_join(dat$survey_samples,
                    select(dat$survey_sets, fishing_event_id, latitude, longitude, depth_m, density_kgpm2 )) %>%
  filter(survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCVI")) %>%
  filter(maturity_code < 7) %>% # remove resting
  filter(sex == 2) %>%
  mutate(mature = ifelse(maturity_code >= 3, 1, 0))

d$density <- d[["density_kgpm2"]] * 1000000
d$log_depth <- log(d$depth_m)

d <- dplyr::filter(d, !is.na(depth_m))
d <- dplyr::filter(d, !is.na(latitude))

d <- sdmTMB::add_utm_columns(d, c("longitude", "latitude"), utm_crs = 32609)

# prep for plotting

nd <- readRDS(file.path("../arrowtooth-nongit", "geostat-figs", "synoptic_grid.rds"))

nd <- na.omit(nd)
nd$log_depth <- log(nd$depth)
nd$age <- 8
nd$length <- 42
nd$sex <- 2
nd$density_kgpm2 <- mean(d$density_kgpm2)
nd$month <- 7

data <- d
coast <- gfplot:::load_coastline(
  range(data$longitude) + c(-0.5, 0.5),
  range(data$latitude) + c(-0.5, 0.5),
  utm_zone = 9
)
coords <- coord_equal(
  expand = FALSE,
  xlim = range(d$X) + c(-100, 50),
  ylim = range(d$Y) + c(-20, 20)
)
utm_labs <- labs(x = "Easting", y = "Northing")

# fit models
mesh1 <- make_mesh(d, c("X", "Y"), cutoff = 20)

fit <- sdmTMB(mature ~ length + s(log_depth) +
              + density_kgpm2
              ,
              spatial_varying = ~ 0 + length,
              data = d,
              mesh = mesh1, family = binomial(), spatial = "on", spatiotemporal = "off" )

s <- sanity(fit)
fit

p <- predict(fit, newdata = nd, type = "response")


p %>%
  #filter(survey != "SYN WCHG") %>%
ggplot(aes(X, Y,
              colour = est
)) +
  coords +
  geom_point(alpha = 1, pch = 15, size = 0.5) +
  geom_polygon(
    data = coast, aes_string(x = "X", y = "Y", group = "PID"),
    fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(trans = "sqrt") +
  scale_colour_viridis_c(trans = "sqrt") +
  labs(
    colour = "Probability\nfemales \nmature at \n42 cm",
    x = tr("Easting"),
    y = tr("Northing")
  ) +
  gfplot::theme_pbs()


# with resting, only the simplest SVC model would fit
# ggsave("spatial-maturity-svc-w-resting.png")

# simple model without resting
# ggsave("spatial-maturity-no-resting.png")
# ggsave("spatial-maturity-svc-no-resting.png")
# ggsave("spatial-maturity-svc-w-depth.png")
ggsave("spatial-maturity-svc-w-covs.png")



p %>%
  #filter(survey != "SYN WCHG") %>%
ggplot( aes(X, Y,
              colour = zeta_s_length
)) +
  coords +
  geom_point(alpha = 1, pch = 15, size = 0.5) +
  geom_polygon(
    data = coast, aes_string(x = "X", y = "Y", group = "PID"),
    fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE
  ) +
  scale_color_gradient2() +
  # scale_fill_viridis_c() +
  # scale_colour_viridis_c() +
  labs(
    colour = "SVC \nprobability\nfemales \nmature at \n42 cm",
    x = tr("Easting"),
    y = tr("Northing")
  ) +
  gfplot::theme_pbs()

ggsave("spatial-maturity-svc-w-covs-devs.png")



# time-varying maturity by age
matage  <- d %>% filter(age >= 0 )

mesh2 <- make_mesh(matage, c("X", "Y"), cutoff = 20)

fit <- sdmTMB(mature ~ age +
              density_kgpm2 + # doesn't do much
              s(log_depth, k = 3)
              ,
              # spatial_varying = ~ 0 + age,
              data = matage,
              mesh = mesh2, family = binomial(),
              spatial = "on", spatiotemporal = "off" )

s <- sanity(fit)

fit

p <- predict(fit, newdata = nd, type = "response")

ggplot(p, aes(X, Y, fill = est, colour = est
  )) +
    coords +
    geom_point(alpha = 1, pch = 15, size = 0.5) +
    geom_polygon(
      data = coast, aes_string(x = "X", y = "Y", group = "PID"),
      fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE
    ) +
    scale_fill_viridis_c(trans = "sqrt") +
    scale_colour_viridis_c(trans = "sqrt") +
    labs(
      fill = "Probability\nfemales \nmature at \nage 8",

      x = tr("Easting"),
      y = tr("Northing")
    ) +
    guides(
      size = guide_legend(order = 1),
      fill = guide_colorbar(order = 0),
      colour = "none"
    ) +
    gfplot::theme_pbs()


  # with resting, only the simplest model would fit
  # ggsave("spatial-maturity-age-w-resting.png")

  # simple model without resting
  # ggsave("spatial-maturity-age-w-depth.png")
  ggsave("spatial-maturity-age-w-covs.png")


# # not working with just females
# ggplot(p, aes(X, Y, fill = (zeta_s_age), colour = (zeta_s_age)
#   )) +
#     coords +
#     geom_point(alpha = 0.5, pch = 21) +
#     geom_polygon(
#       data = coast, aes_string(x = "X", y = "Y", group = "PID"),
#       fill = "grey87", col = "grey70", lwd = 0.2, inherit.aes = FALSE
#     ) +
#     scale_fill_viridis_c() +
#     scale_colour_viridis_c() +
#     labs(
#       fill = "SVC \nprobability\nmature at \nage 8",
#       x = tr("Easting"),
#       y = tr("Northing")
#     ) +
#     guides(
#       size = guide_legend(order = 1),
#       fill = guide_colorbar(order = 0),
#       colour = "none"
#     ) +
#     gfplot::theme_pbs()
#   ggsave("spatial-maturity-svc.png")
