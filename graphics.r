# Arquivo: graphics.r
# Desenha o mapeamento e os locais de teste do Fingerprint
# Autor: Rafael Hengen Ribeiro

graphics <- function(fp, test, fname = 'fp', win = 7, hin = 7)  {
  plot(fp$x, fp$y, type="p", pch=19, col="red", xlab="", ylab="") # Fingerprint
  points(test$correct_x, test$correct_y, pch=25, col="blue", xlab="", ylab="") # Locais de test
  legend("topleft", legend = c("Ponto de referência", "Local de test"), col=c("red","blue"), pch=c(19,25))
  title(main="Local de tests", sub="Fingerprint do laboratório 406B")

  png(filename = paste0(fname,".png"), width = win, height = hin, units = "in", bg = "white", res=100)
}
