# ============================================================
# LATIHAN 6: MEDIATION ANALYSIS LENGKAP (DATASET FRAMING)
# ============================================================
# Load library yang dibutuhkan
install.packages(c("mediation", "diagram", "ggplot2"))

library(mediation)
library(diagram)
library(ggplot2)

# Load dataset framing
data(framing)

names(framing)

# 1. Identifikasi Variabel (X, M, Y)
# X (Treatment): treat (Framing berita)
# M (Mediator): emo (Reaksi emosional negatif)
# Y (Outcome): cong_mes (Mengirim pesan ke Kongres/aksi politik)
# Kovariat: age, educ, gender, income (untuk kontrol confounding)

# 2. Prosedur Baron-Kenny (3 Model Regresi)
cat("=== PROSEDUR BARON-KENNY ===\n")

# Model 1: Total Effect (c) -> X terhadap Y
model.total <- lm(cong_mesg ~ treat + age + educ + gender + income, data = framing)
c_est <- coef(model.total)["treat"]

# Model 2: Path a (X -> M) -> X terhadap M
model.m <- lm(emo ~ treat + age + educ + gender + income, data = framing)
a_est <- coef(model.m)["treat"]

# Model 3: Direct Effect (c') dan Path b (M -> Y)
model.y <- lm(cong_mesg ~ treat + emo + age + educ + gender + income, data = framing)
c_prime_est <- coef(model.y)["treat"]
b_est <- coef(model.y)["emo"]

cat("Path a (X->M):", round(a_est, 3), "\n")
cat("Path b (M->Y):", round(b_est, 3), "\n")
cat("Direct Effect (c'):", round(c_prime_est, 3), "\n")
cat("Total Effect (c):", round(c_est, 3), "\n\n")

# 3. Mediation Analysis dengan Bootstrapping (1000 sims)
set.seed(123) # Untuk hasil yang konsisten
med_hasil <- mediate(model.m, 
                     model.y, 
                     treat = "treat", 
                     mediator = "emo", 
                     boot = TRUE, 
                     sims = 1000)

# 4. Interpretasi Hasil dan Proportion Mediated
summary(med_hasil)

# 5. Sensitivity Analysis
cat("\n=== SENSITIVITY ANALYSIS ===\n")
sens_hasil <- medsens(med_hasil, rho.by = 0.05)
summary(sens_hasil)
plot(sens_hasil, main = "Sensitivity Analysis: ACME vs. Rho")

# 6. Visualisasi Diagram Path
par(mar = c(1, 1, 2, 1))
openplotmat()
# Koordinat node [X, M, Y]
koordinat <- matrix(c(0.1, 0.5, 0.5, 0.9, 0.9, 0.5), ncol = 2, byrow = TRUE)

# Gambar Node
node_labs <- c("Framing\n(X)", "Emosi\n(M)", "Aksi Politik\n(Y)")
for (i in 1:3) {
  textrect(koordinat[i,], 0.12, 0.07, lab = node_labs[i], cex = 0.8)
}

# Gambar Panah dan Koefisien
arr_params <- list(lwd = 2, arr.type = "triangle", arr.length = 0.3)
# Jalur X -> M (a)
straightarrow(from = koordinat[6], to = koordinat[7], lcol = "blue")
text(0.25, 0.78, paste0("a = ", round(a_est, 3)), col = "blue")
# Jalur M -> Y (b)
straightarrow(from = koordinat[7], to = koordinat[8], lcol = "blue")
text(0.75, 0.78, paste0("b = ", round(b_est, 3)), col = "blue")
# Jalur X -> Y (c')
straightarrow(from = koordinat[6], to = koordinat[8], lcol = "red")
text(0.5, 0.45, paste0("c' = ", round(c_prime_est, 3)), col = "red")

title("Diagram Jalur Mediasi Lengkap: Framing -> Emosi -> Aksi")