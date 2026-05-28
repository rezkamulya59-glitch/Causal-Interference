# Latihan 5.1 - Identifikasi Backdoor Path (Konseptual)
# DAG: R -> T -> K dan R -> K

install.packages("dagitty")

library(dagitty)

# Definisikan DAG
dag <- dagitty('dag {
  R -> T
  T -> K
  R -> K
}')

# Visualisasi DAG
plot(dag)

# Daftar path dari R ke K
cat("Semua path dari R ke K:\n")
cat("1. R -> K\n")
cat("2. R -> T -> K\n\n")

# Efek total: tanpa adjustment
total_sets <- adjustmentSets(dag, "R", "K", effect = "total")
print(total_sets)

# Efek direct: mediator T harus dikontrol
direct_sets <- adjustmentSets(dag, "R", "K", effect = "direct")
print(direct_sets)

