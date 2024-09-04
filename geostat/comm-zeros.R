# zeros in commercial data
library(tidyverse)
load_all("../gfplot") # needs arrowtooth branch in order to plot by major area

cpue <- readRDS("../cpue-reports/data/cpue-modern.rds")
# fleet <- readRDS("data/saved_fleet.rds")
# dput(unique(fleet$vessel_registration_number))
# c("30159", "23276", "29196", "29731", "29873", "20881", "29838",
#   "25869", "28001", "23291", "22200", "21948", "29568", "22252",
#   "20093", "29608", "21383", "28120", "21489", "29228", "29863",
#   "29174", "30741", "29518", "20513", "20192", "20925", "30282",
#   "29923", "23277", "29456", "22954", "22460", "25278", "29200",
#   "20815", "20689", "21076", "30629", "29254", "29393", "29560",
#   "21838", "311591")


# make same as discard fleet

d <- tidy_cpue_index(cpue,
  species_common = "arrowtooth flounder",
  gear = "bottom trawl",
  alt_year_start_date = "02-21",
  use_alt_year = TRUE,
  year_range = c(1996, 2021),
  # lat_range = params$lat_range,
  min_positive_tows = 100,
  min_positive_trips = 5,
  min_yrs_with_trips = 5,
  # depth_band_width = 25,
  area_grep_pattern = "^5A|^5B|^5C|^5D|^5E|^3C|^3D"
  # depth_bin_quantiles = params$depth_bin_quantiles,
  # min_bin_prop = 0.001,
  # lat_band_width = 0.1
)
d <- d %>% filter(vessel_registration_number %in% c(
  "30159", "23276", "29196", "29731", "29873", "20881", "29838",
  "25869", "28001", "23291", "22200", "21948", "29568", "22252",
  "20093", "29608", "21383", "28120", "21489", "29228", "29863",
  "29174", "30741", "29518", "20513", "20192", "20925", "30282",
  "29923", "23277", "29456", "22954", "22460", "25278", "29200",
  "20815", "20689", "21076", "30629", "29254", "29393", "29560",
  "21838", "311591"
))
# d2 <- d %>%
#   group_by(year, major_stat_area_code, major_stat_area_description,
#            pos_catch) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   pivot_wider(names_from = "pos_catch", values_from = "n") %>%
#   mutate(`Propotion without arrowtooth` = `0` / (`0` + `1`), total_tows = (`0` + `1`))

d3 <- d %>%
  group_by(year, pos_catch) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = "pos_catch", values_from = "n") %>%
  mutate(`Proportion without arrowtooth` = `0` / (`0` + `1`), total_tows = (`0` + `1`))

max(d3$total_tows)

d3 %>% # filter(year != 2020) %>%
  ggplot() +
  geom_point(aes(year, `Proportion without arrowtooth`
    # colour = major_stat_area_description
  )) +
  geom_smooth(
    method = "gam", aes(year, `Proportion without arrowtooth`
      # colour = major_stat_area_description
    ),
    fill = NA, lty = "dashed"
  ) +
  geom_smooth(
    data = d3, method = "gam", aes(year, `Proportion without arrowtooth`),
    colour = "black",
    fill = "grey"
  ) +
  geom_line(
    data = d3, aes(year, total_tows / max(d3$total_tows)),
    colour = "black",
    lty = "dotted"
  ) +
  scale_colour_viridis_d(direction = -1) +
  scale_fill_viridis_d() +
  ylab("Propotion of tows with no Arrowtooth Flounder") +
  xlab("Year") +
  ggplot2::guides(fill = "none") +
  theme_pbs() +
  theme(legend.title = element_blank())

ggsave("prop-tows-w-0.png")
