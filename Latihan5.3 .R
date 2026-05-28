# Data joint distribution
data <- data.frame(
  X = c("Ya","Ya","Ya","Ya","Tidak","Tidak","Tidak","Tidak"),
  Z = c("Ya","Ya","Tidak","Tidak","Ya","Ya","Tidak","Tidak"),
  Y = c("Ya","Tidak","Ya","Tidak","Ya","Tidak","Ya","Tidak"),
  p = c(0.10, 0.15, 0.05, 0.20, 0.08, 0.12, 0.05, 0.25)
)

# Cek total probabilitas
sum(data$p)

# Marginal P(Z)
pz <- aggregate(p ~ Z, data = data, sum)
pz

# Fungsi P(Y=Ya | X=x, Z=z)
py_given_xz <- function(x, z) {
  sub <- subset(data, X == x & Z == z)
  sub$p[sub$Y == "Ya"] / sum(sub$p)
}

# Backdoor adjustment
p_do_ya <- sum(sapply(unique(data$Z), function(z) {
  py_given_xz("Ya", z) * pz$p[pz$Z == z]
}))

p_do_tidak <- sum(sapply(unique(data$Z), function(z) {
  py_given_xz("Tidak", z) * pz$p[pz$Z == z]
}))

ACE <- p_do_ya - p_do_tidak

# Naive estimates
p_y_given_x_ya <- sum(subset(data, X == "Ya" & Y == "Ya")$p) / sum(subset(data, X == "Ya")$p)
p_y_given_x_tidak <- sum(subset(data, X == "Tidak" & Y == "Ya")$p) / sum(subset(data, X == "Tidak")$p)
naive_diff <- p_y_given_x_ya - p_y_given_x_tidak

# Print results
cat("P(Y=Ya | do(X=Ya)) =", round(p_do_ya, 4), "\n")
cat("P(Y=Ya | do(X=Tidak)) =", round(p_do_tidak, 4), "\n")
cat("ACE =", round(ACE, 4), "\n")
cat("P(Y=Ya | X=Ya) =", round(p_y_given_x_ya, 4), "\n")
cat("P(Y=Ya | X=Tidak) =", round(p_y_given_x_tidak, 4), "\n")
cat("Naive difference =", round(naive_diff, 4), "\n")
cat("Bias (naive - ACE) =", round(naive_diff - ACE, 4), "\n")