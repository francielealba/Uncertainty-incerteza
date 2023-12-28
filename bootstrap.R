assess.uncertainty.6 <- function(sample, model, sampling = T, params = T, equation = "spurrNL"){
  omega <- vcov(model)
  mean <- coef(model)
  ymc <- c()
  beta <-c()
  nSim <- 10000
  
  if (params){
    for (j in 1:nSim) {
      lambdaOmega <- t(chol(omega))
      uncorrelatedErrorsOmega <- matrix(rnorm(length(mean)), ncol=1)
      correlatedErrorsOmega <- lambdaOmega %*% uncorrelatedErrorsOmega
      a <- mean + correlatedErrorsOmega 
      if (equation == "spurr"){
        ymc.this <- a[1]+(sample$d2h)*a[2]
      } else if (equation == "spurrNL"){
        ymc.this <- a[1]*((sample$d2h)^a[2])
      } else if (equation == "schumacher"){
        ymc.this <- a[1]*(sample$dap^a[2])*sample$h^a[3]
      }
      
      ymc <- cbind(ymc, ymc.this)
    }
  }    
  n <- length(unique(sample$Parcela))
  A <- 0.04
  mean <- c()
  varinv <- c()
  
  if (sampling){
    for (i in 1:ncol(ymc)){
      ymc.this <- ymc[,i]
      sample$ymc <- ymc.this 
      volByPlot <- aggregate(sample$ymc ~ sample$Parcela, FUN = sum)[[2]]/A
      mean.this.sim <- mean(volByPlot)
      varDesign.this.sim <- sum((volByPlot - mean.this.sim)**2)/(n*(n - 1))
      mean <- c(mean, mean.this.sim)
      varinv <- c(varinv, varDesign.this.sim)
    }
  } 
  
  varDesign <- sum(varinv)/nSim 
  varParams <- var(mean)
  varTotal <-varDesign+varParams
  varUnder <-(varParams/varTotal)*100
  vardesrel <-100-varUnder
  q <- qt(0.975, n-1)
  IC <- sqrt(varTotal)*q
  media <- mean(mean)/0.04
  
  
  output <- list()
  output$varSampling <- varDesign
  output$varParams <- varParams
  output$varTotal <-varTotal
  output$varUnder <-varUnder
  output$varinv <-vardesrel
  output$IC <-IC
  output$media <-media
  
}

assess.uncertainty.7 <- function(sample, model, sampling = T, params = T, equation = "schumacher"){
  omega <- vcov(model)
  mean <- coef(model)
  ymc <- c()
  beta <-c()
  nSim <- 1000
  
  varSampling <- NA
  varParams <- NA
  varTotal <-NA
  varUnder <-NA
  vardesrel <-NA
  IC <-NA
  media <-NA
  
  if (params){
    for (j in 1:nSim) {
      lambdaOmega <- t(chol(omega))
      uncorrelatedErrorsOmega <- matrix(rnorm(length(mean)), ncol=1)
      correlatedErrorsOmega <- lambdaOmega %*% uncorrelatedErrorsOmega
      a <- mean + correlatedErrorsOmega 
      if (equation == "spurr"){
        ymc.this <- a[1]+(sample$d2h)*a[2]
      } else if (equation == "spurrNL"){
        ymc.this <- a[1]*((sample$d2h)^a[2])
      } else if (equation == "schumacher"){
        ymc.this <- a[1]*(sample$dap^a[2])*sample$h^a[3]
      }
      
      ymc <- cbind(ymc, ymc.this)
    }
  }  
  
  n <- length(unique(sample$Parcela))
  A <- 0.04
  mean <- c()
  varDesign <- c()
  if (sampling){
    for (i in 1:ncol(ymc)){
      ymc.this <- ymc[,i]
      sample$ymc <- ymc.this 
      volByPlot <- aggregate(sample$ymc ~ sample$Parcela, FUN = sum)[[2]]/A
      mean.this.sim <- mean(volByPlot)
      varDesign.this.sim <- sum((volByPlot - mean.this.sim)**2)/(n*(n - 1))
      mean <- c(mean, mean.this.sim)
      varDesign <- c(varDesign, varDesign.this.sim)
    }
  } 
  
  varDesign <- sum(varDesign)/nSim 
  varParams <- var(mean)
  vartotal <-varDesign+varParams
  under <-(varParams/vartotal)*100
  parc <-100-under
  q <- qt(0.975, n-1)
  IC <- sqrt(vartotal)*q
  media <- mean(mean)
  
  output <- list()
  output$varSampling <- varDesign
  output$varParams <- varParams
  output$varTotal <-vartotal
  output$varUnder <-under
  output$varinv <-parc
  output$IC <-IC
  output$media <-media
  
  return(output)
}
