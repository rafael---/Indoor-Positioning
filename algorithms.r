# Arquivo: algorithms.r
# Calcula o erro de vários algoritmos
# Autor: Rafael Hengen Ribeiro

# k-nearest neighbour - (https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm)
# Retorna a média das coordenadas dos K vizinhos mais próximos, ordenados pela distância do sinal
knn <- function(locals, K) {
  apply(head(locals, K)[,c("x","y")], 2, mean)
}

# k-Weighted nearest neighbour
# Similar ao knn, mas returna as coordenadas ponderadas pela distância
kwnn <- function(locals, K)  {
  EPS <- 1e-4 # Constante para evitar a divisão por zero
  nn <- head(locals, K)
  f <- function(n)  sum(1/(nn$distancia+EPS)*n)
  apply(nn[,c("x","y")], 2, f)
}

# Retorna um dataframe contendo todos os erros de todos os algoritmos
calc.error <- function(data, functions) {
  test <- unique(data$id_pos)
  krange <- 2:5

  len <- length(test)

  kn <- c()
  for(f in functions)
    kn <- c(kn, paste0(f, krange))

  error <- as.data.frame(setNames(replicate(length(kn), numeric(len), simplify=FALSE),kn))

  for(t in test) {
    ulocal <- data[data$id_pos == t,]
    local <- ulocal[with(ulocal, order(distancia)), ]
    for(f in functions)  {
      for(k in krange)  {
        result <- do.call(f, list(local, k))
        c <- head(local[, c("correct_x","correct_y")], 1)
        if(c$correct_x != 'NULL')  {
          c <- as.numeric(c)
          error[t, paste0(f,k)] <- sum(abs(c-result))
        }
      }
    }
  }
  return(error)
}
