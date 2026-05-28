library(CausalImpact)
library(zoo)

# 1. Inisialisasi Data Dasar
set.seed(1)
n <- 200
t <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = n)
kontrol_dasar <- cumsum(rnorm(n, 0, 1)) + 50
respons_dasar <- 0.7 * kontrol_dasar + rnorm(n, 0, 2)

# --- SKENARIO 1: IDEAL (Kontrol Tidak Terdampak) ---
# Hanya variabel respons yang naik +8 unit
respons_ideal <- respons_dasar
respons_ideal[150:n] <- respons_ideal[150:n] + 8

data_ideal <- zoo(data.frame(respons = respons_ideal, kontrol = kontrol_dasar), order.by = t)
impact_ideal <- CausalImpact(data_ideal, 
                             pre.period = c(t[1], t[149]),
                             post.period = c(t[150], t[n]))

# --- SKENARIO 2: BIAS (Kontrol Terdampak Intervensi) ---
# Tambahkan pengaruh intervensi pada variabel kontrol (misal naik +5 unit)
respons_bias <- respons_dasar
respons_bias[150:n] <- respons_bias[150:n] + 8
kontrol_bias <- kontrol_dasar
kontrol_bias[150:n] <- kontrol_bias[150:n] + 5 # Pelanggaran asumsi kunci

data_bias <- zoo(data.frame(respons = respons_bias, kontrol = kontrol_bias), order.by = t)
impact_bias <- CausalImpact(data_bias, 
                            pre.period = c(t[1], t[149]),
                            post.period = c(t[150], t[n]))

# 2. Perbandingan Hasil
cat("=== ESTIMASI EFEK: SKENARIO IDEAL ===\n")
cat("Absolute Effect (Average):", round(impact_ideal$summary$AbsEffect[1], 2), "\n\n")

cat("=== ESTIMASI EFEK: SKENARIO BIAS (KONTROL TERDAMPAK) ===\n")
cat("Absolute Effect (Average):", round(impact_bias$summary$AbsEffect[1], 2), "\n")

# Plot untuk melihat perubahan garis kontrafaktual
plot(impact_ideal, main = "Scenario Ideal (Kontrol Bersih)")
plot(impact_bias, main = "Scenario Bias (Kontrol Terdampak)")

# Tambahkan library ggplot2 terlebih dahulu
library(ggplot2)

# Gunakan tanda + untuk menambahkan judul
plot(impact_ideal) + ggtitle("Scenario Ideal (Kontrol Bersih)")
plot(impact_bias) + ggtitle("Scenario Bias (Kontrol Terdampak)")

summary(impact_ideal) 
summary(impact_bias) 