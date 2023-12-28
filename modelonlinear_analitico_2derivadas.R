###funçao de derivadas####

shumbio.w <-gnls(BT~b0*(d^b1)*(h^b2),weights =  varPower(.1, form = ~ BT), data = biomassa, start = list(b0=0.1, b1=1.8, b2=1.2))
Spurr.wb <-nls(BT~b0*I(d^2*h)^b1, weights = pesos, data=biomassa, start = list(b0=0.1, b1=1.8))


inventariovol <- read.table("C:/Doutorado/Processamento Cunia - tese/Volume/inventario.txt", header = T)
sample <-inventariovol

sample$predVol <- coef(Spurrall.wb)[1]*((sample$d2h)^coef(Spurrall.wb)[2])


test <- assess.uncertainty.5(sample, model = Spurrall.wb, sampling = T, params = T, equation = "schumacher")


assess.uncertainty.5 <- function(sample, model, sampling = T, params = T, equation = "schumacher"){
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
      sample$db3 <- 0
      sample$db4 <- beta[1]*(sample$h**beta[3])*(sample$dap**(beta[2]))*log(sample$dap)*log(sample$dap)
      sample$db5 <- beta[1]*sample$dap**(beta[2])*(sample$h**beta[3])*log(sample$h)*log(sample$h)
      sample$db6 <- 0
      sample$db7 <- beta[1]*(sample$h**beta[3])*(sample$dap**(beta[2]))*log(sample$dap)*log(sample$dap)*log(sample$dap)
      sample$db8 <- beta[1]*sample$dap**(beta[2])*(sample$h**beta[3])*log(sample$h)*log(sample$h)*log(sample$h)
      z1 <-tapply(sample$db0,sample$Parcela, FUN = sum)/A
      z2 <-tapply(sample$db1,sample$Parcela, FUN = sum)/A
      z3 <-tapply(sample$db2,sample$Parcela, FUN = sum)/A
      z4 <-tapply(sample$db3,sample$Parcela, FUN = sum)/A
      z5 <-tapply(sample$db4,sample$Parcela, FUN = sum)/A
      z6 <-tapply(sample$db5,sample$Parcela, FUN = sum)/A
      z7 <-tapply(sample$db6,sample$Parcela, FUN = sum)/A
      z8 <-tapply(sample$db7,sample$Parcela, FUN = sum)/A
      z9 <-tapply(sample$db8,sample$Parcela, FUN = sum)/A
      zm1 <-mean(z1)
      zm2 <-mean(z2)
      zm3 <-mean(z3)
      zm4 <-mean(z4)
      zm5 <-mean(z5)
      zm6 <-mean(z6)
      zm7 <-mean(z7)
      zm8 <-mean(z8)
      zm9 <-mean(z9)
      J <-cbind(zm1,zm2,zm3)
      m <-cbind(zm1,zm4,zm2,zm5,zm3,zm6)
      M <-matrix(m, nrow = 2, ncol = 3)
      p <-cbind(zm1,zm4,zm7,zm2,zm5,zm8,zm3,zm6,zm9)
      P <-matrix(p, nrow = 3, ncol = 3)
    } else if (equation == "spurr"){
      sample$db0 <- (sample$d2h)^beta[2]
      sample$db1 <-  beta[1]*(sample$d2h)^beta[2]*log(sample$d2h)
      sample$db2 <- 0
      sample$db3 <-  beta[1]*(sample$d2h)^beta[2]*log(sample$d2h)*log(sample$d2h)
      z1 <-tapply(sample$db0,sample$Parcela, FUN = sum)/A
      z2 <-tapply(sample$db1,sample$Parcela, FUN = sum)/A
      z3 <-tapply(sample$db2,sample$Parcela, FUN = sum)/A
      z4 <-tapply(sample$db3,sample$Parcela, FUN = sum)/A
      d <-mean(z1)
      zm2 <-mean(z2)
      zm3 <-mean(z3)
      zm4 <-mean(z4)
      J <-cbind(d,zm2)
      m <-cbind(d,zm3,zm2,zm4)
      M <-matrix(m, nrow = 2, ncol = 2)
    }
    
    # Variance due to the parameters (tree level)
    varPar1 <- J %*% vcov(model) %*% t(J)
    varPar2 <- (sum(M %*% vcov(model))^2)*1/2 
    ##varPar3 <- (sum(P %*% vcov(model)^3))*1/6 
    varParams <- varPar1+varPar2
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

