N_t <- length(seq(1997, 2021))
yrs <- seq(1997, 2021)
N_a <- 20
N_t_real <- N_t
N_s <- 2
N_fleets <- 2

projected_N <- 0
N_t <- N_t + projected_N

# TURN_OFF_F <- FALSE
TURN_OFF_F <- TRUE

# female first
M <- c(0.20, 0.35)
lw_a <- c(0.0000076, 0.0000095)
lw_b <- c(3.0515274, 2.9741834)
k <- c(0.1815153, 0.273536)
t0 <- c(-0.478988,  -0.2580476)
linf <- c(61.7695997, 47.1587886)
age50 <- c( 5.5656931, 4.1025745)
sd50 <- c(1/0.9109433, 1/1.2466718)
# sd50 <- c(0.9109433, 1.2466718)

# Table 6 freezer trawlers; female first
a_hat_freezer <- c(7.97, 7.35)
gamma_hat_freezer <- c(1.02, 0.89)
a_hat_shoreside <- c(8.67, 7.94)
gamma_hat_shoreside <- c(1.06, 0.96)
a_hat <- list(a_hat_freezer, a_hat_shoreside)
gamma_hat <- list(gamma_hat_freezer, gamma_hat_shoreside)

# F input:
projected_F_fr <- rep(0, projected_N)
projected_F_sh <- rep(0, projected_N)

# dput(round(as.numeric(apply(d$mcmc$ft[[1]], 2, mean)), 4))
freezer <- c(0, 0, 0, 0, 0.0013, 0.0004, 0.0007, 0.0002, 0, 0.0246, 0.0544,
  0.0172, 0.0289, 0, 0.0021, 0.0379, 0.0418, 0.0997, 0.1644, 0.1499,
  0.1719, 0.1763, 0.1794, 0.155, 0.0248, 0.0641)[-1]

freezer <- c(freezer, projected_F_fr)

if (TURN_OFF_F) freezer <- rep(0, length(freezer))

# shoreside <- c(
#   0.0691955, 0.095285, 0.102809, 0.0987223, 0.1504135, 0.1209875,
#   0.1117555, 0.15482, 0.31474, 0.092989, 0.0609828, 0.05350525,
#   0.05793275, 0.044355, 0.06985625, 0.05331015, 0.0497313, 0.0440276,
#   0.04182915, 0.05743625, 0.06577925, 0.0391806, 0.04350635, 0.02848705,
#   0.0414128
# )
# dput(round(as.numeric(apply(d$mcmc$ft[[2]], 2, mean)), 4))

shoreside <- c(0.115, 0.0697, 0.0958, 0.1031, 0.099, 0.1506, 0.1213, 0.1119,
  0.1551, 0.3157, 0.093, 0.0614, 0.054, 0.0582, 0.0446, 0.07, 0.0536,
  0.0501, 0.0443, 0.0421, 0.0575, 0.0663, 0.0394, 0.0438, 0.0287,
  0.0417)[-1]
shoreside <- c(shoreside, projected_F_sh)
if (TURN_OFF_F) shoreside <- rep(0, length(shoreside))

# recruitment input:
# mean(d$mcmc$params$rbar)
R_init <- 63.16045 # table 6
R_bar <- 85.83149 # table 6

# R_init <- 34
# R_bar <- 78.80596

# dput(round(as.numeric(apply(d$mcmc$rdev, 2, mean)), 4))
omegas <- c(0.2089, 0.1333, 0.4203, 0.693, 0.5867, 0.5181, 0.4759, 0.2707,
  0.1185, 0.1696, 0.2744, 0.3054, -0.0864, -0.1191, -0.3735, 0.0288,
  -0.3072, -0.0869, -0.674, -1.0435, -0.4563, -0.3483, -0.6476,
  -0.0085, -0.0394)

omegas_proj <- rep(0, projected_N)
omegas <- c(omegas, omegas_proj)
plot(omegas, type = "o")

init_omegas <- c(
  0.524497,0.34968,0.128145,0.474558,0.577389,0.797567,0.727452,
  0.884873,0.744107,0.241596,-0.145522,0.148283,-0.471437,-0.675812,-1.43043,
  -0.0210956,0.153862,-1.07244,-1.93528
)
plot(init_omegas, type = "o")

age <- 1:N_a
l_a <- matrix(nrow = N_a, ncol = N_s)
w_a <- matrix(nrow = N_a, ncol = N_s)
mat_a <- matrix(nrow = N_a, ncol = N_s)
f_a <- matrix(nrow = N_a, ncol = N_s)
v_a <- matrix(nrow = N_a, ncol = N_s)

# length at age:
for (s in 1:N_s) {
  l_a[,s] <- linf[s] * (1 - exp(-k[s] * (age - t0[s]))) # G17
}
matplot(age, l_a)

# weight at age:
for (s in 1:N_s) {
  w_a[,s] <- lw_a[s] * l_a[,s]^lw_b[s] # G18
}
matplot(l_a, w_a)

# maturity at age:
for (s in 1:N_s) {
  mat_a[,s] <- plogis(age, age50[s], sd50[s])
}
matplot(age, mat_a)

# fecundity at age:
for (a in 1:N_a) {
  for (s in 1:N_s) {
    f_a[a,s] <- w_a[a,s] * mat_a[a,s]
  }
}
matplot(age, f_a)

# apical F:
F_ta <- list()
for (f in 1:N_fleets) F_ta[[f]] <- matrix(nrow = N_t, ncol = N_a)
for (f in 1:N_fleets) {
  for (a in 1:N_a) {
    F_ta[[f]][, a] <- if (f == 1) freezer else shoreside
  }
}
matplot(F_ta[[1]], type = "l")
matplot(F_ta[[2]], type = "l")

# vulnerability at age:
v_a <- list()
for (f in 1:N_fleets) v_a[[f]] <- matrix(nrow = N_a, ncol = N_s)

for (f in 1:N_fleets) {
  for (s in 1:N_s) {
    v_a[[f]][,s] <- 1 / (1 + exp(-(age - a_hat[[f]][s]) / gamma_hat[[f]][s]))
  }
}
matplot(v_a[[1]])
matplot(v_a[[2]])

# total mortality at age:
Z_ta <- list()
for (s in 1:N_s) Z_ta[[s]] <- matrix(nrow = N_t, ncol = N_a)

for (t in 1:N_t) {
  for (s in 1:N_s) {
    for (a in 1:N_a) {
      total_f <- 0
      for (f in 1:N_fleets) {
        total_f <- total_f + F_ta[[f]][t, a] * v_a[[f]][a, s]
      }
      Z_ta[[s]][t, a] <- M[s] + total_f
    }
  }
}

# numbers at age and SSB:
N_ta <- list()
for (s in 1:N_s) N_ta[[s]] <- matrix(nrow = N_t, ncol = N_a, data = 0)
SSB_ta <- list()
for (s in 1:N_s) SSB_ta[[s]] <- matrix(nrow = N_t, ncol = N_a, data = 0)

# initialize numbers at age and SSB in first time step
for (s in 1:N_s) { # T5.4 iscam docs
  ii <- 0
  for (t in 1) {
    for (a in 2:N_a) {
      ii <- ii + 1
      N_ta[[s]][t, a] <- R_init * exp(init_omegas[ii]) * exp(-M[s])^(a - 1) / N_s
    }
  }
}

# initialize recruits for all other years
for (s in 1:N_s) {
  for (t in 1:N_t) {
    for (a in 1) {
      N_ta[[s]][t, a] <- R_bar * exp(omegas[t]) / N_s # T 5.5 iscam docs
    }
  }
}

# fill in rest of numbers at age
for (s in 1:N_s) {
  for (t in 2:N_t) {
    for (a in 2:N_a) { # T 5.12 iscam docs
      N_ta[[s]][t, a] <- N_ta[[s]][t - 1, a - 1] * exp(-Z_ta[[s]][t - 1, a - 1])
      if (a == N_a) { # plus group
        N_ta[[s]][t, a] <- N_ta[[s]][t, a] +
          N_ta[[s]][t - 1, a - 1] * exp(-Z_ta[[s]][t - 1, a])
      }
    }
  }
}

# calculate SSB
SSB_t <- rep(0, N_t)
for (s in 1:N_s) {
  for (t in 1:N_t) {
    for (a in 1:N_a) {
      SSB_ta[[s]][t, a] <- N_ta[[s]][t, a] * f_a[a, s]
    }
    SSB_t[t] <- SSB_t[t] + sum(SSB_ta[[s]][t,])
  }
}

plot(SSB_t, type = "o", ylim = c(0, 160))
abline(h = 150, lty = 2)
abline(h = 70, lty = 2)

# mean(d$mcmc$params$sbo)
SSB0 <- 185.4
plot(SSB_t/SSB0, type = "o", ylim = c(0, 1))
# abline(h = 1, lty = 2)
abline(h = 0.4, lty = 2)
abline(h = 0.2, lty = 2)

if (TURN_OFF_F) SSB_t_noF <- SSB_t

if (FALSE) {
  plot(yrs, SSB_t/SSB0, type = "l", ylim = c(0, 1))
  # abline(h = 1, lty = 2)
  abline(h = 0.4, lty = 2)
  abline(h = 0.2, lty = 2)
  lines(yrs, SSB_t_noF/SSB0, col = "grey60")

  library(ggplot2)
  dat <- data.frame(year = yrs, SSB = SSB_t / SSB0,
    SSB_dynB0 = SSB_t_noF / SSB0)
  # dat <- data.frame(year = yrs, SSB = SSB_t / SSB0,
  #   SSB_dynB0 = SSB_t_noF)
  ggplot(dat, aes(year, SSB)) +
    geom_line() +
    geom_line(aes(y = SSB_dynB0), col = "blue") +
    gfplot::theme_pbs() +
    ylab("Relative spawning biomass") +
    xlab("Year") +
    coord_cartesian(ylim = c(0, 1.05), expand = FALSE) +
    geom_hline(yintercept = 0.4, lty = 2, colour = "darkgreen") +
    geom_hline(yintercept = 0.2, lty = 1, colour = "red")
    # scale_y_continuous(breaks = seq(0.12, 1.2, 0.12))
  ggsave("doc/figure/dynamic-B0.png", width = 7, height = 4)
 }

# # calculate catch
# C_ta <- list()
# C_ta[[1]] <- matrix(nrow = N_t, ncol = N_a, data = 0)
# C_ta[[2]] <- matrix(nrow = N_t, ncol = N_a, data = 0)
#
# SSB_t <- rep(0, N_t)
# for (t in 1:N_t) {
#   for (a in 1:N_a) {
#     C_ta[t, a] <- (N_ta[t, a] * w_a[a] * F_ta[t, a] *
#         v_a[a] * (1 - exp(-Z_ta[t, a]))) / Z_ta[t, a]
#   }
# }
# C_t <- apply(C_ta, 1, sum)
# plot(C_t, type = "o")
#
# # calculate vulnerable biomass
# V_ta <- matrix(nrow = N_t, ncol = N_a)
# lambda <- 0
# for (t in 1:N_t) {
#   for (a in 1:N_a) {
#     V_ta[t, a] <- N_ta[t, a] *
#       exp(-lambda * Z_ta[t, a]) * v_a[a] * w_a[a]
#   }
# }
# V_t <- apply(V_ta, 1, sum)
# plot(V_t, type = "o")
#
# # calculate total biomass
# B_ta <- matrix(nrow = N_t, ncol = N_a)
# for (t in 1:N_t) {
#   for (a in 1:N_a) {
#     B_ta[t, a] <- N_ta[t, a] * w_a[a]
#   }
# }
# B_t <- apply(B_ta, 1, sum)
#
# # plot
# cols <- RColorBrewer::brewer.pal(4, "Dark2")
#
# plot(1:N_t, SSB_t,
#   type = "l", ylab = "T", xlab = "Year",
#   ylim = c(0, max(B_t)), col = cols[1]
# )
# lines(1:N_t, V_t, col = cols[2], lty = 1)
# lines(1:N_t, C_t, col = cols[3], lty = 2)
# lines(1:N_t, B_t, col = cols[4], lty = 1)
# legend("bottomleft",
#   legend = c("SSB", "VB", "Catch", "B"),
#   lty = c(1, 1, 1, 1), col = cols
# )
# abline(v = N_t_real, lty = 2)
