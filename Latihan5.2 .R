# 5.2 Latihan 2: Simpson's Paradox

library(ggplot2)
library(dplyr)
library(dagitty)
library(ggdag)

# Data 
data_studi <- data.frame(
  jenis_kelamin = c("Pria", "Pria", "Wanita", "Wanita"),
  treatment     = c("Ya", "Tidak", "Ya", "Tidak"),
  sembuh        = c(70, 60, 80, 60),
  total         = c(100, 100, 100, 100)
)

# 1. Persentase Marginal
cat("\nPersentase Marginal\n")
marginal <- aggregate(cbind(Sembuh, Total) ~ Treatment, data = df, sum)
marginal$PersenSembuh <- marginal$Sembuh / marginal$Total * 100
marginal

# 2. Persentase per Jenis Kelamin 
data_studi$persen <- data_studi$sembuh / data_studi$total * 100

cat("\nPersentase per Jenis Kelamin\n")
print(data_studi[, c("jenis_kelamin", "treatment", "persen")])

# 3. Cek Simpson's paradox
cat("Marginal Ya =", 150/200*100, "%\n")
cat("Marginal Tidak =", 120/200*100, "%\n")
cat("Pria Ya =", 70/100*100, "%\n")
cat("Pria Tidak =", 60/100*100, "%\n")
cat("Wanita Ya =", 80/100*100, "%\n")
cat("Wanita Tidak =", 60/100*100, "%\n")

# Interpretasi sederhana
if ((150/200) > (120/200) && (70/100) > (60/100) && (80/100) > (60/100)) {
  cat("\nTidak ada Simpson's paradox pada data ini.\n")
} else {
  cat("\nAda kemungkinan Simpson's paradox.\n")
}

# 4. DAG Confounder 
dag_conf <- dagitty('dag {
  JenisKelamin [pos="1,0"]
  Treatment    [pos="0,1"]
  Sembuh       [pos="2,1"]
  JenisKelamin -> Treatment
  JenisKelamin -> Sembuh
  Treatment    -> Sembuh
}')

ggdag(dag_conf) +
  theme_dag_blank() +
  labs(title = "DAG 1: Jenis Kelamin sebagai Confounder")

# 4.DAG Mediator
dag_med <- dagitty('dag {
  Treatment    [pos="0,0"]
  JenisKelamin [pos="1,0"]
  Sembuh       [pos="2,0"]
  Treatment    -> JenisKelamin
  JenisKelamin -> Sembuh
  Treatment    -> Sembuh
}')

ggdag(dag_med) +
  theme_dag_blank() +
  labs(title = "DAG 2: Jenis Kelamin sebagai Mediator")