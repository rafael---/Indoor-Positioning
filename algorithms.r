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
  f <- function(n)  sum(n/(nn$dist+EPS))
  apply(nn[,c("x","y")], 2, f)
}

# Distância euclidiana do sinal
distance <- function(fingerprint, local)  {
  aps <- apply(local, 2, mean)

  len <- length(fingerprint$x)

  dist <- data.frame(x = numeric(len), y = numeric(len), dist = numeric(len))

  for(i in 1:len)  {
    dist$x[i] <- fingerprint$x[i]
    dist$y[i] <- fingerprint$y[i]
    #dist$dist[i] <- sqrt((aps['ap1'] - fingerprint$ap1[i])^2+(aps['ap2'] - fingerprint$ap2[i])^2+(aps['ap3'] - fingerprint$ap3[i])^2)  #Distancia euclidiana
    dist$dist[i] <- 0
    for(j in 1:3)
        dist$dist[i] <- dist$dist[i] + (aps[paste0('ap',j)] - fingerprint[i,paste0('ap',j)])^2 # Distancia Euclidiana
    dist$dist[i] <- sqrt(dist$dist[i])
  }
  dist <- dist[with(dist, order(dist)),]
}

# Retorna um dataframe contendo todos os erros de todos os algoritmos
calc.error <- function(data, functions) {
  test <- unique(data$id_pos)
  krange <- 3:4

  len <- length(test)

  kn <- c()
  for(f in functions)
    kn <- c(kn, paste0(f, krange))

  error <- as.data.frame(setNames(replicate(length(kn), numeric(len), simplify=FALSE),kn))

  for(t in test) {
    ulocal <- data[data$id_pos == t,]
    local <- distance(fingerprint,ulocal)
    print(local)
    for(f in functions)  {
      for(k in krange)  {
        result <- do.call(f, list(local, k))
        c <- head(ulocal[, c("correct_x","correct_y")], 1)
        if(c$correct_x != 'NULL' & !is.na(c$correct_x))  {
          c <- as.numeric(c)
          error[t, paste0(f,k)] <- sum(abs(c-result))
          print(paste0("ID: ",t," - ",f,k," - Error:",sum(abs(c-result))," - Real (",c[1],",",c[2],") - M: (",result[1],",",result[2],")"))
          print(paste0("KNN:",head(local,k)))
        }
      }
    }
  }
  return(error)
}
