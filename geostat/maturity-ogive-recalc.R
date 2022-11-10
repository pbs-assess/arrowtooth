# optim maturity

# library(viridis)
library(patchwork)

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

d1 <- save_ogive_dataframe(`Synoptic only`,
  maturities_kept = c(1, 2, 3, 4, 5, 6, 7)
)
d2 <- save_ogive_dataframe(`All samples`,
  maturities_kept = c(1, 2, 3, 4, 5, 6, 7)
)
d3 <- save_ogive_dataframe(`All samples`,
  maturities_kept = c(1, 2, 3, 4, 5, 6)
)
d4 <- save_ogive_dataframe(`All samples`,
  maturities_kept = c(1, 4, 5, 6)
)
# d5 <- save_ogive_dataframe(`Synoptic only`,
#   maturities_kept = c(1, 4, 5, 6)
# )

mat_dat <- bind_rows(d1, d2, d3, d4) %>% mutate(
  model = case_when(
    maturities_kept == paste0(deparse(c(1, 2, 3, 4, 5, 6, 7))) ~ paste(data),
    maturities_kept == paste0(deparse(c(1, 2, 3, 4, 5, 6))) ~ paste(data, "\n(resting removed)"),
    maturities_kept == paste0(deparse(c(1, 4, 5, 6))) ~ paste(data, "\n(maturing, developing \nand resting removed)")
    # maturities_kept == paste0(deparse(c(1, 2, 3, 4, 5, 6))) ~ "Resting removed",
    # maturities_kept == paste0(deparse(c(1, 4, 5, 6))) ~ "Maturing, developing \nand resting removed"
  ),
  sex = ifelse(sex == 1L, "Male", "Female")
)
unique(mat_dat$model)
mat_dat$model <- factor(mat_dat$model, levels = c(
  "Synoptic only",
  "All samples",
  "All samples \n(resting removed)",
  "All samples \n(maturing, developing \nand resting removed)"
  # "Resting removed",
  # "Maturing, developing \nand resting removed"
  # "Synoptic only \n(maturing, developing \nand resting removed)"
), labels = c(
  "Synoptic only",
  "All samples",
  # "All samples \n(resting removed)",
  # "All samples \n(maturing, developing \nand resting removed)"
  "Resting removed",
  "Maturing, developing \nand resting removed"
  # "Synoptic only \n(maturing, developing \nand resting removed)"
))

mat_dat$sex <- factor(mat_dat$sex, levels = c("Male", "Female"))
age <- 1:40

# female_cols <- c("#FC8D62", "#0096FF", "#4682B4", "black", "red")
# female_cols <- plasma(4)
# female_cols <- inferno(5)

library(RColorBrewer)
# female_cols <- brewer.pal(n = 6, name = "Reds")
female_cols <- brewer.pal(n = 8, name = "YlOrRd")
female_cols <- female_cols[5:8]
female_cols <- rev(female_cols)

# male_cols <- brewer.pal(n = 6, name = "Blues")
male_cols <- brewer.pal(n = 6, name = "GnBu")
male_cols <- male_cols[3:6]
male_cols <- rev(male_cols)




plot_bridging_mat_ogives <- function(mat_dat,
                                     plot_sex = "Female",
                                     col_vec = female_cols,
                                     ogive_size = 1.3,
                                     ogive_alpha = 0.4) {

  models <- unique(mat_dat$model)

  p <- mat_dat %>%
    filter(sex == !!plot_sex) %>%
    ggplot() +
    geom_point(aes(age, prop_mature, colour = model),
      # shape = 1,
      alpha = 0.7
    ) +
    scale_color_manual(values = col_vec, name = "Model") +
    ylab("Proportion mature") +
    xlab("Age") +
    ggtitle(plot_sex) +
    ggsidekick::theme_sleek()


  add_ogive <- function(p, i){
  p + geom_function(
    data = NULL,
    size = ogive_size,
    alpha = ogive_alpha,
    # lty="dashed",
    fun = function(x) {
      plogis(
        x,
        mat_dat[mat_dat$model == models[i] & mat_dat$sex == plot_sex, ]$age50mat[1],
        mat_dat[mat_dat$model == models[i] & mat_dat$sex == plot_sex, ]$sd50mat[1]
      )
    }, colour = col_vec[i], inherit.aes = FALSE
  ) +
    geom_vline(
      data = NULL,
      size = ogive_size / 2, lty = "dashed", alpha = ogive_alpha, xintercept =
        mat_dat[mat_dat$model == models[i] &
                  mat_dat$sex == plot_sex, ]$age50mat[1], colour = col_vec[i]
    )
    }

  for (i in seq_along(models)){
    p <- p %>% add_ogive(i)
  }

  p
}

p1 <- plot_bridging_mat_ogives(mat_dat, plot_sex = "Male", col_vec = male_cols)
# + theme(
#   legend.title = element_blank())
p2 <- plot_bridging_mat_ogives(mat_dat, plot_sex = "Female", col_vec = female_cols) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank())

# xlab <- p1$labels$x

xlab <- ggplot(data.frame(l = "Age", x = 13, y = 1)) +
  geom_text(aes(x, y, label = l))+
  theme_void()


(p1 + p2 )/xlab +
  plot_layout(heights = c(20, 1.5)) & coord_cartesian(expand = F, xlim = c(0, 26)) & theme(
  legend.title = element_blank(),
  axis.title.x = element_blank(),
  legend.position = c(0.7, 0.25)
)

ggsave("maturities.png", width = 8, height = 3.5)

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
