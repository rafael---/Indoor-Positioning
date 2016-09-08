# Arquivo: main.r
# Calcula o erro médio e desvio padrão de vários algoritmos e desenha o mapeamento do Fingerprint
# Autor: Rafael Hengen Ribeiro

source("algorithms.r")
source("graphics.r")

fingerprint <- read.csv("input/list_local.csv",header= TRUE, stringsAsFactors=FALSE)
data <- read.csv("input/posicionamento.csv",header= TRUE, stringsAsFactors=FALSE)

functions <- c("knn", "kwnn")

error <- calc.error(data, functions)

for(c in colnames(error)) {
  print(paste0("Mean absolute error ",c,": ",mean(error[, c(c)])," m"))
  print(paste0("Standard Deviation ",c,": ",sd(error[, c(c)])," m\n"))
}

graphics(fingerprint, unique(data[, c("correct_x","correct_y")]))
