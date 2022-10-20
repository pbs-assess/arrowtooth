N_t <- length(seq(1997, 2021))
N_a <- 40

M <- 0.2
lw_a <- 0.0000076
lw_b <- 3.0515274

lw_a <- exp(-11.77)
lw_b <- 3.04
k <- 0.1815
t0 <- -0.4789
linf <- 61.77

age50 <- 5.56
sd50 <- 0.91

age <- 1:N_a

l_a <- linf * (1 - exp(-k * (age - t0))) # G17
plot(age, l_a)

w_a <- lw_a * l_a^lw_b # G18
plot(l_a, w_a)

mat_a <- plogis(age, 5.5, 0.91)
f_a <- w_a * mat_a
plot(age, f_a)

freezer <- c(
  0.000000000000774678, 0.000000000000723788, 0.0000000000008278765,
  0.001336905, 0.00039389, 0.000669733, 0.0002219715, 0.000005693365,
  0.0245305, 0.05421945, 0.0171112, 0.0286268, 0.00002605795, 0.002070115,
  0.0376067, 0.0415481, 0.09910165, 0.1635095, 0.1490125, 0.1706155,
  0.175452, 0.1784755, 0.1530835, 0.02461195, 0.06330635
)

shoreside <- c(
  0.0691955, 0.095285, 0.102809, 0.0987223, 0.1504135, 0.1209875,
  0.1117555, 0.15482, 0.31474, 0.092989, 0.0609828, 0.05350525,
  0.05793275, 0.044355, 0.06985625, 0.05331015, 0.0497313, 0.0440276,
  0.04182915, 0.05743625, 0.06577925, 0.0391806, 0.04350635, 0.02848705,
  0.0414128
)

F_both <- freezer + shoreside
# F_both <- F_both
F_ta <- matrix(nrow = N_t, ncol = N_a)
for (a in 1:N_a) {
  F_ta[, a] <- F_both
}

# Table 6 freezer trawlers female selectivity
a_hat <- 7.97
gamma_hat <- 1.02

# shoreside
a_hat <- 8.7
gamma_hat <- 1.06
v_a <- 1 / (1 + exp(-(age - a_hat) / gamma_hat))
plot(v_a)

Z_ta <- matrix(nrow = N_t, ncol = N_a)
for (t in 1:N_t) {
  for (a in 1:N_a) {
    Z_ta[t, a] <- M + F_ta[t, a] * v_a[a]
  }
}

R_bar <- 63.1 # table 6 rbar init
N_ta <- matrix(nrow = N_t, ncol = N_a)

recdevs <- c(
  0.2163865, 0.1364665, 0.4195205, 0.697793, 0.585176, 0.5183845,
  0.474046, 0.268234, 0.1226545, 0.170575, 0.2780495, 0.3062655,
  -0.0846347, -0.1129565, -0.369483, 0.02906495, -0.310351, -0.0828904,
  -0.669309, -1.02718, -0.44503, -0.32971, -0.6275755, -0.0339801,
  -0.0487646
)
plot(recdevs, type = "o")

N_ta[, 1] <- R_bar * exp(recdevs)
for (t in 1) {
  for (a in 2:N_a) {
    N_ta[t, a] <- R_bar * exp(0) * exp(-M)^(a - 1)
    # should be exp(recdevs[t - a]) but just using mean recdevs
  }
}

for (t in 2:N_t) {
  for (a in 2:N_a) {
    N_ta[t, a] <- N_ta[t - 1, a - 1] * exp(-Z_ta[t - 1, a - 1])
  }
}

C_ta <- matrix(nrow = N_t, ncol = N_a)
for (t in 1:N_t) {
  for (a in 1:N_a) {
    C_ta[t, a] <- (N_ta[t, a] * w_a[a] * F_ta[t, a] *
      v_a[a] * (1 - exp(-Z_ta[t, a]))) / Z_ta[t, a]
  }
}

C_t <- apply(C_ta, 1, sum)
plot(C_t, type = "o")

V_ta <- matrix(nrow = N_t, ncol = N_a)

lambda <- 0
for (t in 1:N_t) {
  for (a in 1:N_a) {
    V_ta[t, a] <- N_ta[t, a] *
      exp(-lambda * Z_ta[t, a]) * v_a[a] * w_a[a]
  }
}

V_t <- apply(V_ta, 1, sum)
plot(V_t, type = "o")

SSB_ta <- matrix(nrow = N_t, ncol = N_a)
for (t in 1:N_t) {
  for (a in 1:N_a) {
    SSB_ta[t, a] <- N_ta[t, a] * f_a[a]
  }
}
SSB_t <- apply(SSB_ta, 1, sum)

B_ta <- matrix(nrow = N_t, ncol = N_a)
for (t in 1:N_t) {
  for (a in 1:N_a) {
    B_ta[t, a] <- N_ta[t, a] * w_a[a]
  }
}
B_t <- apply(B_ta, 1, sum)

SSB_t <- apply(SSB_ta, 1, sum)

cols <- RColorBrewer::brewer.pal(4, "Dark2")

plot(1:N_t, SSB_t,
  type = "l", ylab = "T", xlab = "Year",
  ylim = c(0, max(B_t)), col = cols[1]
)
lines(1:N_t, V_t, col = cols[2], lty = 1)
lines(1:N_t, C_t, col = cols[3], lty = 2)
lines(1:N_t, B_t, col = cols[4], lty = 1)
legend("topright",
  legend = c("SSB", "VB", "Catch", "B"),
  lty = c(1, 1, 1, 1), col = cols
)
