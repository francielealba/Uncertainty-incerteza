####Analitycal approach####

####exemplo###
AjusteSpurr <- lm(v~ I(d^2*h), data = volume)
spurrbio <-lm(BT~I(DAP^2*h), data = biomassa)
spurrbio.w <-lm(BT~I(DAP^2*h), weights = linpesos, data = biomassa)


inventariovol <- read.table("C:/Doutorado/Processamento Cunia - tese/Volume/inventario.txt", header = T)
sample <-inventariovol

sample$predVol <- coef(spurrbio.w)[1]+coef(spurrbio.w)[2]*((sample$d2h))


test<- assess.uncertainty.2(sample, model = spurrbio.w, sampling = T, params = T, equation = "Linearspurr")
View(test)
Resut1 <-test

assess.uncertainty.2 <- function(sample, model, sampling = T, params = T, equation = "Linearspurr"){
  beta <- coef(model)
  A <- 0.04
  N <- nrow(sample)
  n <- length(unique(sample$Parcela))
  sbb <-vcov(model)
  
  vardes <- NA
  varmodel <- NA
  vartotal <-NA
  varunder <-NA
  vardesrel<-NA
  IC <-NA
  media <-NA
  
  if (sampling){
    Sh1P <- tapply(sample$n, sample$Parcela, FUN =sum)/A
    Sh2P <- tapply(sample$d2h, sample$Parcela, FUN =sum)/A
    Z1P <-mean(Sh1P)
    z2P <- mean(Sh2P)
    S11P <- (sum((Sh1P-Z1P)^2)/n-1)/n
    S12P <- ((sum((Sh1P-Z1P)*(Sh2P-z2P)))/n-1)/n
    S22P <- (sum((Sh2P-z2P)^2)/n-1)/n
    Sz <- data.frame(S11P, S12P, S12P, S22P)
    Sz1 <-as.numeric(Sz)
    Szz <- matrix(Sz1, ncol = 2, nrow = 2)
    vardes<- t(beta)%*%Szz%*%beta
  }
  if (params){
    if (equation == "Linearspurr"){
      Z <-cbind(Z1P,z2P)
      varmodel <-(Z)%*%sbb%*%t(Z)}
  }
  
  vartotal <-vardes+varmodel
  varunder <-(varmodel/vartotal)*100
  vardesrel <-100-varunder
  q <- qt(0.975, n-1)
  IC <- sqrt(vartotal)*q
  media <-Z%*%(beta)
  
  output <- list()
  output$varSampling <- vardes
  output$varParams <- varmodel
  output$varTotal <-vartotal
  output$varUnder <-varunder
  output$varinv <-vardesrel
  output$IC <-IC
  output$media <-media
  
  return(output)
}
