# Arquivo: graphics.r
# Desenha o mapeamento e os locais de teste do Fingerprint
# Autor: Rafael Hengen Ribeiro

valid <- data.frame(x = numeric(64), y = numeric(64))

c = 0
for (i in 0:12*3) {
  for(j in 0:6*3) {
    if(i > 3 & j > 3 & i < 33 & j < 15)
      next
    c <- c +1
    valid$x[c] = i
    valid$y[c] = j
  }
}


routers <- data.frame(x = c(7.5,18,28.5), y = c(0.5,17.5,0.5))

graphics <- function(fp, test, fname = 'graphics/fp', win = 14, hin = 7)  {
  fp[, c("y")] <- abs(fp[, c("y")])
  test[, c("correct_y")] <- abs(test[, c("correct_y")])
  png(filename = paste0(fname,".png"), width = win, height = hin, units = "in", bg = "white", res=100)
  plot(valid$x, valid$y, type="p", pch=1, col="black", xlab="", ylab="", xlim=c(0,36),ylim=c(0,18))
  points(fp$x, fp$y, type="p", pch=19, col="red", xlab="", ylab="") # Fingerprint
  points(routers$x, routers$y, type="p", pch=8, col="green", xlab="", ylab="") # Roteadores
  points(test$correct_x, test$correct_y, pch=25, col="blue", xlab="", ylab="") # Locais de teste
  legend(4,14, legend = c("Ponto de referência", "Local de teste","Roteador (AP)","Ponto não mapeado"), col=c("red","blue","green","black"), pch=c(19,25,8,1))
  title(main="Ambiente de testes", sub="Fingerprint (parcial) do 4o. andar")
}
