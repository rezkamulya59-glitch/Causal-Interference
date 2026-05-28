# ============================================================
# Latihan 4: Counterfactual Individu (Kasus Sari)
# ============================================================

# 1. Data Observasi Sari (Faktual)
usia_sari <- 35
x_aktual  <- 1
y_aktual  <- 20

# 2. Langkah 1: Abduction
# Mencari nilai Uy untuk Sari berdasarkan persamaan: Y = 12*X - 0.1*Usia^2 + Uy
uy_sari <- y_aktual - (12 * x_aktual - 0.1 * (usia_sari^2))

# 3. Langkah 2 & 3: Action dan Prediction
# Skenario: Seandainya Sari TIDAK minum obat (X = 0)
x_cf <- 0
y_kontrafaktual <- (12 * x_cf) - 0.1 * (usia_sari^2) + uy_sari

# 4. Hitung Individual Treatment Effect (ITE)
ite_sari <- y_aktual - y_kontrafaktual

# 5. Menghitung ATE Populasi (Secara Teoretis)
# E[Y|do(X=1)] - E[Y|do(X=0)] dengan asumsi E[Uy] = 0
y_do_1 <- (12 * 1) - 0.1 * (usia_sari^2) + 0
y_do_0 <- (12 * 0) - 0.1 * (usia_sari^2) + 0
ate_populasi <- y_do_1 - y_do_0

# Menampilkan Hasil Lengkap
cat("==================================================\n")
cat("HASIL ANALISIS KONTRAFAKTUAL LATIHAN 4\n")
cat("==================================================\n")
cat("1. Nilai Uy (Abduction)        : ", uy_sari, "\n")
cat("2. Nilai Y Aktual (Faktual)    : ", y_aktual, "\n")
cat("3. Nilai Y Kontrafaktual (X=0) : ", y_kontrafaktual, "\n")
cat("4. ITE (Individual Effect)     : ", ite_sari, "\n")
cat("5. ATE (Population Effect)     : ", ate_populasi, "\n")
cat("==================================================\n")