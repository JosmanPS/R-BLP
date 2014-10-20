#In this script we create the objects that the original that the matlab code
#has in 

#Set the working directory
setwd("/Users/josmanps/REPOS/BLP-3/BLP-3")            #home wd


#===================================================================================
#PACKAGES
#load the packages needed
library("R.matlab")
library("reshape")
#===================================================================================

#===================================================================================
#DATA
#Get the example data of Nevo

#Instrumental Variables
iv <- readMat("Data/iv.mat")
iv <- iv$iv
#===================================================================================

#===================================================================================
#Get the data like Nevo used.

ps2 <- readMat("Data/ps2.mat")

#id - an id variable in the format bbbbccyyq, where bbbb is a unique 4 digit identifier for each brand, 
#cc is a city code, yy is year (=88 for all observations is this data set) and q is quarter. All the
#other variables are sorted by date city brand.
id <- ps2$id

#id_demo - an id variable for the random draws and the demographic variables, of the format
#ccyyq. Since these variables do not vary by brand they are not repeated.
id_demo <- ps2$id.demo

#s_jt - the market shares of brand j in market t. Each row corresponds to the equivalent row in id.
s_jt <- ps2$s.jt

#x1 - the variables that enter the linear part of the estimation (we must include a constant). 
#This consist of price variable and dummy variables.
#Each row corresponds to the equivalent row in id.
x1 <- as.matrix(ps2$x1)

#x2 - the variables that enter the non-linear part of the estimation.
#Consists of constant, price, and characterisitcs.
#Each row corresponds to the equivalent row in id.
x2 <- ps2$x2

#v - random draws given for the estimation.
#for each individual there is a different draw for each column of x2.
#The ordering is given by id_demo.
v <- ps2$v

#demogr - draws of demographic variables from the CPS for 20 individuals in each market.
#Each of the variables has been demeaned.
#The ordering is given by id_demo.
demogr <- ps2$demogr

#===================================================================================

#===================================================================================
#Data brands

#IndMerc

mkt <- substr(id, 5, 9)

IndMerc <- rep(1,length(id))
aux <- mkt[1]
aux2 <- 1

for(i in 1:length(id)){
  if(mkt[i] == aux){
    IndMerc[i] <- aux2
  }else{
    aux2 <- aux2 + 1
    IndMerc[i] <- aux2
    aux <- mkt[i]
  }	
}
mkt <- IndMerc

#IndBrand
nmkt <- 94     # number of markets = (# of cities)*(# of quarters)  %
nbrn <- 24 
brand <- rep(1:24, 94)

#Acomodamos la base de datos
cereal <- data.frame(id, mkt, brand, x2, s_jt)
names(cereal) <- c("id", "mkt", "brand", "constant", "price", "sugar", "mushy", "S_jt")



rm("id", "x2", "brand", "mkt", "s_jt", "IndMerc", "aux", "aux2", "ps2", "nbrn")
#===================================================================================

#===================================================================================
#Demographics

#Arreglaremos los datos de modo que sean visualmente mas accesibles

v1 <- c()
v2 <- c()
v3 <- c()
v4 <- c()
for(i in 1:dim(demogr)[1]){
  v1 <- c(v1, demogr[i, 1:20])
  v2 <- c(v2, demogr[i, 21:40])
  v3 <- c(v3, demogr[i, 41:60])
  v4 <- c(v4, demogr[i, 61:80])
}

ind <- rep(1:20, nmkt)
mkt <- rep(1:94, each = 20)

#Simulamos las perturbaciones demograficas
set.seed(130056)
sd1 <- rnorm(length(v1))
sd2 <- rnorm(length(v1))
sd3 <- rnorm(length(v1))
sd4 <- rnorm(length(v1))


demogr <- data.frame(ind, mkt, v1, v2, v3, v4, sd1, sd2, sd3, sd4)
names(demogr) <- c("ind", "mktD", "income", "sq.income", "age", "child", "sd1", "sd2", "sd3", "sd4")

rm("mkt", "nmkt", "v1", "v2", "v3", "v4", "id_demo", "v", "sd1", "sd2", "sd3", "sd4")
#===================================================================================

# **********************************************************************************

#===================================================================================
#Anadimos las variables de los datos de las empresas que son necesarias para la interaccion
#con demograficas.
priceD <- cast(data.frame(cereal$mkt, cereal$brand, cereal$price), cereal.mkt ~ cereal.brand)
sugarD <- cast(data.frame(cereal$mkt, cereal$brand, cereal$sugar), cereal.mkt ~ cereal.brand)
mushyD <- cast(data.frame(cereal$mkt, cereal$brand, cereal$mushy), cereal.mkt ~ cereal.brand)

priceD <- priceD[,-1]
sugarD <- sugarD[,-1] 
mushyD <- mushyD[,-1]
  
priceD2 <- matrix(0, dim(demogr)[1], dim(priceD)[2])
sugarD2 <- priceD2
mushyD2 <- priceD2

for(j in 1:dim(priceD2)[2]){
  priceD2[,j] <- rep(priceD[,j], each=max(ind))
  sugarD2[,j] <- rep(sugarD[,j], each=max(ind))
  mushyD2[,j] <- rep(mushyD[,j], each=max(ind))
}

priceD <- priceD2
sugarD <- sugarD2
mushyD <- mushyD2

rm("priceD2", "sugarD2", "mushyD2")
#===================================================================================

#==========================================================================================================
#MARKET SIZE

#We define the market size for this specific data

ns <- 20       # number of simulated "indviduals" per market %
nmkt <- 94     # number of markets = (# of cities)*(# of quarters)  %
nbrn <- 24     # number of brands per market. if the numebr differs by
#market this requires some "accounting" vector %
# Default is ns=20, nmkt=94, nbrn=24. Thus we have 24*94=2256 observations
#Set this smaller if you want to have more a dataset you can eyeball better

n_inst <- 20   #Number of instruments for price

#==========================================================================================================

#==========================================================================================================
#INSTRUMENTAL VARIABLES

IV <- cbind(iv[,2:(n_inst+1)], x1[,2:(nbrn+1)])

# The previous line creates a matrix IV from the instruments and the x's.
#20 columns of instruments for price by default, and nbrn brand dummies


IV <- IV[1:(nmkt*nbrn), ]
Z <- IV
rm("IV")
# The previous line  reduces the data matrix, e.g.,  x1, to its first  nmkt*nbrn observations;

#We create the weight matrix

phi <- t(Z) %*% Z 

#Creamos la matriz con las variables de la parte lineal
X <- cbind(1, cereal$price, cereal$sugar, cereal$mushy)


attach(cereal)
attach(demogr)

#==========================================================================================================


