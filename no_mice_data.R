
#####################################################################################################################

# without mice FA




No_miceData <- read.csv("final_No_mice.csv")

##Factor Analysis

#Data without question 19
data_without19 <- No_miceData[,-(61:66)]
#we delete col 19 as it is not that much important

#Now we also dont need demographic as per requirement of project
data<-data_without19[,-(1:5)]

decidefactor <- fa.parallel(data,fm ='ml', fa = 'fa')
#According to parallel analysis we have 11 factors

#As we see by the parallel analysis number of factor should be 11.

#Try 1
#Third model with 12 factors
factana7 <- fa(data,nfactors =12)
fa.diagram(factana7)

library(psych)
fa.diagram(factana7$loadings)

install.packages("GPArotation")
library(GPArotation)
factana8 <- fa(data,nfactors = 11)
fa.diagram(factana8)

#keeping only correlated variables keeping cutoff 0.1
install.packages("caret")
library('caret')
data_corelatedNoMice = findCorrelation(cor(data), cutoff=0.3)
data_corelatedNoMice
hc1= sort(data_corelatedNoMice)
data_corelatedNoMice = data[, c(hc1)]
dim(data_corelatedNoMice)
#We are left with only 29 variables.


# trying with 11 Factors 
factana9 <- fa(data_corelatedNoMice,nfactors = 11)
fa.diagram(factana9)


# Trying with 10 factors
factanal10 <- factanal(data_corelatedNoMice, factors = 10)
factanal10

factana10 <- fa(data_corelatedNoMice,nfactors =10)

fa.diagram(factana10)


#trying with 9 factors 
factana11 <- fa(data_corelatedNoMice,nfactors =9)
fa.diagram(factana11)

factanal11 <- factanal(data_corelatedNoMice, factors = 9)
factanal11



colnames(factana11$loadings)
colnames(factana11$loadings) <- c("RecBeh",
                                  "MngRspb",
                                  "Rspb",
                                  "SocAfct",
                                  "HltPrcp",
                                  "EnvCons",
                                  "EnvMntlst",
                                  "PercBeh",
                                  "Knldge")

fa.diagram(factana11)