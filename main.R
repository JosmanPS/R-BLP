#==========================================================================================================
#                                    BLP REGRESSION WITH DEMOGRAPHICS
#                                   Jose Manuel Proudinat Silva (2014)
#==========================================================================================================

#==========================================================================================================

#Set the working directory
#setwd("D:/User Profiles/T41542/Documents/BLP-2") #Banxico wd
setwd("/Users/josmanps/REPOS/BLP-3/BLP-3") #MAC wd

#==========================================================================================================
#PACKAGES

#Packages used for convergencies
#Fuente: http://www.r-project.org/conferences/useR-2010/abstracts/Reynaerts+Varadhan+Nash.pdf
#http://www.econ.kuleuven.be/VIVES/publicaties/discussionpapers/DP/DP2012/discussion-paper-35.pdf

library("pracma") 

#==========================================================================================================

#INITIAL VALUES

#The vector of linear estimators is as follow:
#constant, price, sugar, mushy(dummy)

#The vector of non-linear estimators is as follow:
#constant (sd)
#int : constant * income
#int : constant * age
#price (sd)
#int : price * income
#int : price * income^2
#int : price * child
#sugar (sd)
#int : sugar * income
#int : sugar * age
#mushy (sd)
#int : mushy * income
#int : mushy * age



#These are the initial values, they are commented so we can use different initial values, but these are the suggested.
thet2 <- c(0.3303, 5.4819, 0.2037, 2.4526, 15.8935, -1.2, 2.6342, 0.0163, -0.2506, 0.0511, 0.2441, 1.2650, -0.8091)



#now we have the data and the initial values of theta2 and delta, we can proceed with the estimation
#==========================================================================================================

#Cargamos las funciones necesarias
source("conmap.blp.R")
source("predict.blp.R")
source("objective.blp.R")
source("GMMvalue.blp.R")

new.thet2 <- nelder_mead(thet2, GMMvalue.blp, maxfeval = 150)
#1374. con valores iniciales y tol de conmap 1e-01
#My last thet2 = c(0.01246784,  5.37683560,  0.34073521,  2.44649898, 15.87608684, -0.85889799,
  # 2.67057364, -0.07006780, -0.29084388, 0.03504236,  0.20125009,  0.95930296, -0.84897894)
#10 de Agosto 2014, 20:35
