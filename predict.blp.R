predict.blp <- function(t2, delta){
  
  # INPUT
  # delta : current value of delta vector
  # t2 : current value of Pi vector (from outer loop)
  # OUTPUT
  # s_ijt : matrix of individual choice probabilities for i = 1, ..., n.s
  # pred.share : aggregate market shares
  
  
  #Acomodamos delta en una tabla que facilite el algoritmo
  deltaD <- cast(data.frame(cereal$mkt, cereal$brand, delta), cereal.mkt ~ cereal.brand, value = "delta")
  deltaD <- deltaD[,-1]
  
  deltaD2 <- matrix(0, dim(demogr)[1], dim(deltaD)[2])
  
  for(j in 1:dim(deltaD)[2]){
    deltaD2[,j] <- rep(deltaD[,j], each=max(ind))
  }
  deltaD <- deltaD2
  rm("deltaD2")
  
  
  #We compute the individual probabilities
  s_ijt <- matrix(0, ns*nmkt, nbrn)
  
  #Calculamos el numerador
  for(j in 1:nbrn){
    s_ijt[,j] <- exp(deltaD[,j] + 1*(t2[1]*sd1 + t2[2]*income + t2[3]*age) 
                     + priceD[,j]*(t2[4]*sd2 + t2[5]*income + t2[6]*sq.income + t2[7]*child)
                     + sugarD[,j]*(t2[8]*sd3 + t2[9]*income + t2[10]*age)
                     + mushyD[,j]*(t2[11]*sd4 + t2[12]*income + t2[13]*age))
  }
  
  #Calculamos el denominador
  denom <- 1 + rowSums(s_ijt)
  
  #Dividimos para obtener las probabilidades individuales
  for(j in 1:nbrn){
    s_ijt[,j] <- s_ijt[,j]/denom
  }
  
  #Obtenemos el promedio para obtener las probabilidades agregadas
  pred.share <- rep(0, dim(cereal)[1])
  for(i in 1:nmkt){
    for(j in 1:nbrn){
      pred.share[j + nbrn*(i-1)] <- mean(s_ijt[1:20 + 20*(i-1),j])
    }
  }
  
  list(s_ijt = s_ijt, pred.share = pred.share)
  
}