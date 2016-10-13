# Signal strength propagation models
# Developer: Rafael Hengen Ribeiro
# Reference:
# Chen, Yongguang, and Hisashi Kobayashi. "Signal strength based indoor geolocation."
#   Communications, 2002. ICC 2002. IEEE International Conference on. Vol. 1. IEEE, 2002.
# ----------------------------------------------------------------------------------------
# INPUT: Expects a CSV file with columns distance and rssi
# ----------------------------------------------------------------------------------------
# Input example: input/samples_example.csv

DEFAULT_ALPHA_LOS <- 2.0
DEFAULT_ALPHA_NLOS <- 3.3

rssi_simple_model <- function(dist, initial_rssi, alpha=DEFAULT_ALPHA_LOS, initial_dist=1)  {
  rssi <- initial_rssi - 10*alpha*log10(dist/initial_dist)
}

estimate_parameters <- function(samples, dist, initial_dist = 1) {
  rho <- -10*log10(dist/initial_dist)
  mrho <- mean(rho)
  len <- length(samples)
  ssum <- 0 -> dsum
  for(i in 1:len) {
    ssum <- ssum + (rho[i] - mrho)*samples[i]
    dsum <- dsum + (rho[i]-mrho)^2
  }
  estimated_alpha <- ssum/dsum
  initial_rssi <- mean(samples) - estimated_alpha*mrho
  data.frame(alpha = estimated_alpha, initial_rssi)
}

draw_graphics <- function(csv_name) {
  data <- read.csv(csv_name, header= TRUE, stringsAsFactors=FALSE)
  parameters <- estimate_parameters(data$rssi, data$dist)

  simple_LOS <- rssi_simple_model(data$dist, parameters$initial_rssi)
  simple_NLOS <- rssi_simple_model(data$dist, parameters$initial_rssi, DEFAULT_ALPHA_NLOS)

  vars <- c(parameters$rssi,simple_LOS,simple_NLOS)

  plot(data$dist, type="n", xlab="Distância (em metros)", ylab="RSSI (em dBm)", xlim=c(0,max(data$dist)), ylim=c(min(vars),max(vars)))
  points(data$dist, simple_LOS, type="o", col="green", pch=19)
  points(data$dist, simple_NLOS, type="o", col="blue", pch=19)
  points(data$dist, data$rssi, type="o", col="red", pch=19)
  legend("topright",legend=c("Modelo simples LOS","Modelo simples NLOS", "Modelo de propagação UFFS"), col=c("green","blue","red"), pch=19)
  title("Comparação de modelos de propagação do sinal")
}

if(!interactive())  {
  draw_graphics("input/samples_example.csv")
}
