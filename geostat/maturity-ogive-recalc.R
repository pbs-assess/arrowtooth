# optim maturity

all_samples <- bind_rows(survey_samples, commercial_samples)

mat_model <- function(par, age, prop_mature) {
  prop_m <- 1 / (1 + exp(-(age - par[1]) / par[2]))
  sum((prop_m - prop_mature)^2) # sum of squares
}

save_ogive_dataframe <- function(data, maturities_kept) {
  # sex <-2
  matlst <- list()
  obj <- list()
  for (sex in 1:2) {
    # browser()
    matlst[[sex]] <- data %>%
      filter(maturity_code %in% maturities_kept) %>%
      filter(sex == !!sex) %>%
      filter(!is.na(age)) %>%
      group_by(age) %>%
      mutate(is_mature = ifelse(maturity_code < 3, FALSE, TRUE)) %>%
      summarize(prop_mature = sum(is_mature) / n()) %>%
      ungroup()

    # browser()
    obj[[sex]] <- optim(
      par = c(7, 1),
      fn = mat_model,
      method = "L-BFGS-B",
      lower = 0.001,
      upper = Inf,
      age = matlst[[sex]]$age,
      prop_mature = matlst[[sex]]$prop_mature
    )
    # browser()
    matlst[[sex]]$sex <- sex
    matlst[[sex]]$age50mat <- round(obj[[sex]]$par[1], 2)
    matlst[[sex]]$sd50mat <- round(obj[[sex]]$par[2], 2)
    matlst[[sex]]
  }
  df <- do.call("rbind", matlst)

  df$data <- deparse(substitute(data))
  df$maturities_kept <- paste0(deparse(maturities_kept))
  df
}


# deparse(substitute(survey_samples))
# check <- all_samples %>% filter(age == 14)

`Synoptic only` <- survey_samples_syn %>% filter(age < 41)
`All samples` <- all_samples %>% filter(age < 41)

d1 <- save_ogive_dataframe(`synoptic only`,
  maturities_kept = c(1, 2, 3, 4, 5, 6, 7)
)
d2 <- save_ogive_dataframe(`all samples`,
  maturities_kept = c(1, 2, 3, 4, 5, 6, 7)
)
d3 <- save_ogive_dataframe(`all samples`,
  maturities_kept = c(1, 2, 3, 4, 5, 6)
)
d4 <- save_ogive_dataframe(`all samples`,
  maturities_kept = c(1, 4, 5, 6)
)
d5 <- save_ogive_dataframe(`synoptic only`,
  maturities_kept = c(1, 4, 5, 6)
)

mat_dat <- bind_rows(d1, d2, d3, d4, d5) %>% mutate(
  model = case_when(
    maturities_kept == paste0(deparse(c(1, 2, 3, 4, 5, 6, 7))) ~ paste(data),
    # maturities_kept == paste0(deparse(c(1, 2, 3, 4, 5, 6))) ~ paste(data, "\n(resting removed)"),
    # maturities_kept == paste0(deparse(c(1, 4, 5, 6))) ~ paste(data, "\n(maturing, developing \nand resting removed)")
    maturities_kept == paste0(deparse(c(1, 2, 3, 4, 5, 6))) ~ "Resting removed",
    maturities_kept == paste0(deparse(c(1, 4, 5, 6))) ~ "Maturing, developing \nand resting removed"
  ),
  sex = ifelse(sex == 1L, "Male", "Female")
)
unique(mat_dat$model)
mat_dat$model <- factor(mat_dat$model, levels = c(
  "Synoptic only",
  "All samples",
  # "all samples \n(resting removed)",
  # "all samples \n(maturing, developing \nand resting removed)"
  "Resting removed",
  "Maturing, developing \nand resting removed"
  # "synoptic only \n(maturing, developing \nand resting removed)"
))

mat_dat$sex <- factor(mat_dat$sex, levels = c("Male", "Female"))
age <- 1:40

cols <- c( "#FC8D62", "#0096FF", "#4682B4", "black", "red")

p1 <- mat_dat %>%
  filter(sex == "Male") %>%
  ggplot() + geom_point(aes(age, prop_mature, colour = model),
    # shape = 1,
    alpha = 0.5
  ) +
  # geom_function(fun = ~ plogis(.x, .$age50mat[1], .$sd50mat[1])) +
  geom_function(fun = function(x) {
    plogis(x,
      mat_dat[mat_dat$model == "synoptic only" & mat_dat$sex == "Male", ]$age50mat[1],
      mat_dat[mat_dat$model == "synoptic only" & sex == 1, ]$sd50mat[1]
    )
  }, colour = cols[1], inherit.aes = FALSE) +
  geom_function(fun = function(x) {
    plogis(x,
           mat_dat[mat_dat$model == "all samples" & mat_dat$sex == "Male", ]$age50mat[1],
           mat_dat[mat_dat$model == "all samples" & sex == 1, ]$sd50mat[1]
    )
  }, colour = cols[2], inherit.aes = FALSE) +
  geom_function(fun = function(x) {
    plogis(x,
           mat_dat[mat_dat$model == "all samples \n(resting removed)" & mat_dat$sex == "Male", ]$age50mat[1],
           mat_dat[mat_dat$model == "all samples \n(resting removed)" & sex == 1, ]$sd50mat[1]
    )
  }, colour = cols[3], inherit.aes = FALSE) +
  geom_function(fun = function(x) {
    plogis(x,
           mat_dat[mat_dat$model == "all samples \n(maturing, developing \nand resting removed)"
                   & mat_dat$sex == "Male", ]$age50mat[1],
           mat_dat[mat_dat$model == "all samples \n(maturing, developing \nand resting removed)"
                   & sex == 1, ]$sd50mat[1]
    )
  }, colour = cols[4], inherit.aes = FALSE) +
  # geom_function(fun = function(x) {
  #   plogis(x,
  #          mat_dat[mat_dat$model == "synoptic only \n(maturing, developing \nand resting removed)"
  #                  & mat_dat$sex == "Male", ]$age50mat[1],
  #          mat_dat[mat_dat$model == "synoptic only \n(maturing, developing \nand resting removed)"
  #                  & sex == 1, ]$sd50mat[1]
  #   )
  # }, colour = cols[5], inherit.aes = FALSE) +
  # # scale_color_viridis_d(direction = -1) +
  scale_color_manual(values = cols) +
  ylab("Proportion mature") +
  xlab("Age") +
  # facet_grid(~sex) +
  ggtitle("Male") +
  ggsidekick::theme_sleek() +
  theme(legend.position = "none",
        axis.title.x = element_blank())


p2 <- mat_dat %>%
  filter(sex == "Female") %>%
  ggplot() + geom_point(aes(age, prop_mature, colour = model),
                        # shape = 1,
                        alpha = 0.5
  ) +
  # geom_function(fun = ~ plogis(.x, .$age50mat[1], .$sd50mat[1])) +
  geom_function(fun = function(x) {
    plogis(x,
           mat_dat[mat_dat$model == "synoptic only" & mat_dat$sex == "Female", ]$age50mat[1],
           mat_dat[mat_dat$model == "synoptic only" & sex == 1, ]$sd50mat[1]
    )
  }, colour = cols[1], inherit.aes = FALSE) +
  geom_function(fun = function(x) {
    plogis(x,
           mat_dat[mat_dat$model == "all samples" & mat_dat$sex == "Female", ]$age50mat[1],
           mat_dat[mat_dat$model == "all samples" & sex == 1, ]$sd50mat[1]
    )
  }, colour = cols[2], inherit.aes = FALSE) +
  geom_function(fun = function(x) {
    plogis(x,
           mat_dat[mat_dat$model == "all samples \n(resting removed)" & mat_dat$sex == "Female", ]$age50mat[1],
           mat_dat[mat_dat$model == "all samples \n(resting removed)" & sex == 1, ]$sd50mat[1]
    )
  }, colour = cols[3], inherit.aes = FALSE) +
  geom_function(fun = function(x) {
    plogis(x,
           mat_dat[mat_dat$model == "all samples \n(maturing, developing \nand resting removed)"
                   & mat_dat$sex == "Female", ]$age50mat[1],
           mat_dat[mat_dat$model == "all samples \n(maturing, developing \nand resting removed)"
                   & sex == 1, ]$sd50mat[1]
    )
  }, colour = cols[4], inherit.aes = FALSE) +
  # geom_function(fun = function(x) {
  #   plogis(x,
  #          mat_dat[mat_dat$model == "synoptic only \n(maturing, developing \nand resting removed)"
  #                  & mat_dat$sex == "Female", ]$age50mat[1],
  #          mat_dat[mat_dat$model == "synoptic only \n(maturing, developing \nand resting removed)"
  #                  & sex == 1, ]$sd50mat[1]
  #   )
  # }, colour = cols[5], inherit.aes = FALSE) +
  ## scale_color_viridis_d(direction = -1) +
  scale_color_manual(values = cols, name = "Model") +
  ylab("Proportion mature") +
  xlab("Age") +
  # facet_grid(~sex) +
  ggtitle("Female") +
  ggsidekick::theme_sleek() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank())

library(patchwork)

p1 + p2 + plot_layout()

# # all synoptic survey data
# mat_a <- plogis(age, 5.5622846, 0.9170395)
# plot(age, mat_a, col = "black", type = "l")
# points(allsyn$prop_mature ~ allsyn$age, col = "black")
#
# # all data
# mat_all <- plogis(age, 5.473380, 1.067196)
# lines(age, mat_all, col = "blue", type = "l")
# points(alldat$prop_mature ~ alldat$age, col = "blue")
#
# # # all data with random effect
# # mat_a <- plogis(age, 5.4, 1.2)
# # plot(age, mat_a)
#
# #  all data, no resting #123456
# mat_no7 <- plogis(age, 7.5324880, 1.666116)
# lines(age, mat_no7, col = "red", type = "l")
# points(allno7$prop_mature ~ allno7$age, col = "red")
#
# # # just synoptic, no resting #123456
# # mat_synno7 <- plogis(age, 7.806608, 1.657300)
# # lines(age, mat_synno7, col = "purple", type = "l")
# # points( synno7$prop_mature~synno7$age, col = "purple")
#
# # 13456
# mat_all13456 <- plogis(age, 5.2815355, 0.6062867)
# lines(age, mat_all13456, col = "orange", type = "l")
# points(all13456$prop_mature ~ all13456$age, col = "orange")
#
# # #12456
# # mat_all12456 <- plogis(age, 9.816071, 2.176438)
# # lines(age, mat_all12456, col = "purple", type = "l")
# # points( all12456$prop_mature~all12456$age, col = "purple")
#
#
# # # 1456 ith random effect = knife edged
# # mat_a <- plogis(age, 7, 0.2)
# # plot(age, mat_a)
#
# # 1456 = new optim
# mat_all1456 <- plogis(age, 6.267387, 0.7097857)
# lines(age, mat_all1456, col = "green")
# points(all1456$prop_mature ~ all1456$age, col = "green", pch = 16)
#
# # # just synoptic
# # mat_syn1456 <- plogis(age, 6.2346493, 0.6650841)
# # lines(age, mat_syn1456, col = "grey", type = "l")
# # points( syn1456$prop_mature~syn1456$age, col = "grey")
#









# export_mat_lw_age(all_samples,
#                   surv_abbrevs = c("HS MSA",    "MSSM QCS",  "MSSM WCVI", "OTHER",
#                                    "SYN HS",    "SYN QCS",   "SYN WCHG",  "SYN WCVI",
#                                    NA),
#                   # areas here are 3CD and 5ABCDE in order
#                   areas = c("03",
#                             "04",
#                             "05",
#                             "06",
#                             "07",
#                             "08",
#                             "09"))


## original
# 5.5134759 4.1311211 # -age at 50% maturity
# 0.9014744 1.2335712 # -std at 50% maturity

# 6.1854784 4.841369 # -age at 50% maturity
# 1.4649476 1.2035514 # -std at 50% maturity
