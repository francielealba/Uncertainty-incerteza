####Função método analítico (fran) n linear#####

shumbio.w <-gnls(BT~b0*(d^b1)*(h^b2),weights =  varPower(.1, form = ~ BT), data = biomassa, start = list(b0=0.1, b1=1.8, b2=1.2))
Spurr.wb <-nls(BT~b0*I(d^2*h)^b1, weights = pesos, data=biomassa, start = list(b0=0.1, b1=1.8))


inventariovol <- read.table("C:/Doutorado/Processamento Cunia - tese/Volume/inventario.txt", header = T)
sample <-inventariovol

sample$predVol <- coef(Spurr.wb)[1]*((sample$dap)^2*(sample$h))^coef(Spurr.wb)[2]


test <- assess.uncertainty.4(sample, model = Spurr.wb, sampling = T, params = T, equation = "spurr")


assess.uncertainty.4 <- function(sample, model, sampling = T, params = T, equation = "spurr"){
  beta <- coef(model)
  A <- 0.04
  N <- nrow(sample)
  n <- length(unique(sample$Parcela))
  b <-matrix(coef(model))
  
  varSampling <- NA
  varParams <- NA
  varTotal <-NA
  varUnder <-NA
  media <-NA
  
  if (sampling){
    vol.plot <<- aggregate(sample$predVol ~ sample$Parcela, FUN = sum)[[2]]/A
    mean.vol <- mean(vol.plot)
    varSampling <- sum((vol.plot - mean.vol)**2)/(n*(n - 1))
  }
  if (params){
    if (equation == "schumacher"){
      sample$db0 <- (sample$dap**beta[2])*(sample$h**beta[3])
      sample$db1 <- beta[1]*(sample$h**beta[3])*(sample$dap**(beta[2]))*log(sample$dap)
      sample$db2 <- beta[1]*sample$dap**(beta[2])*(sample$h**beta[3])*log(sample$h)
      z1 <-tapply(sample$db0,sample$Parcela, FUN = sum)/A
      z2 <-tapply(sample$db1,sample$Parcela, FUN = sum)/A
      z3 <-tapply(sample$db2,sample$Parcela, FUN = sum)/A
      zm1 <-mean(z1)
      zm2 <-mean(z2)
      zm3 <-mean(z3)
      J <-cbind(zm1,zm2,zm3)
    } else if (equation == "spurr"){
      sample$db0 <- (sample$d2h)^beta[2]
      sample$db1 <-  beta[1]*(sample$d2h)^beta[2]*log(sample$d2h)
      z1 <-tapply(sample$db0,sample$Parcela, FUN = sum)/A
      z2 <-tapply(sample$db1,sample$Parcela, FUN = sum)/A
      zm1 <-mean(z1)
      zm2 <-mean(z2)
      J <-cbind(zm1,zm2)
    }
    
    # Variance due to the parameters (tree level)
    
    varParams <- J %*% vcov(model) %*% t(J)
  }
  
  varTotal <-varSampling+varParams 
  varUnder <-(varParams/varTotal)*100
  media <-
    
    output <- list()
  output$varSampling <- varSampling
  output$varParams <- varParams
  output$varTotal <-varTotal
  output$varUnder <-varUnder
  output$media <-media
  
  return(output)
}
