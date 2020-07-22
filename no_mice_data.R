


No_miceData <- read.csv("final_No_mice.csv")




#PCA
res.pca0 <- prcomp(No_miceData[1:5],scale= TRUE)  
summary(res.pca0)
plot(res.pca0)
screeplot(res.pca0,type="line",main="Scree Plot")
library("factoextra")
eig.val0 <- get_eigenvalue(res.pca0)
eig.val0

dimT0 <- c(1:5)

#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT0, eig.val0$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")

#StreePlot
fviz_eig(res.pca0)

#Loading score
fviz_pca_var(res.pca0,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


# A. KNOWLEGDE
res.pca1 <- prcomp(No_miceData[6:19],scale= TRUE)  
summary(res.pca1)

library("factoextra")
eig.val1 <- get_eigenvalue(res.pca1)
eig.val1

dimT1 <- c(1:14)

#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT1, eig.val1$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")

#StreePlot
fviz_eig(res.pca1)

#Loading score
fviz_pca_var(res.pca1,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))



# B. ATTITUDE AND PERCEPTION
names(No_miceData[20:26])
res.pca2 <- prcomp(No_miceData[20:26],scale= TRUE)  
summary(res.pca2)

library("factoextra")
eig.val2 <- get_eigenvalue(res.pca2)
eig.val2

dimT2 <- c(1:7)
dimT2

#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT2, eig.val2$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")

#StreePlot
fviz_eig(res.pca2)

#Loading Score
fviz_pca_var(res.pca2,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


# c. SOCIAL NORMS AFFECTING PLASTIC CHANGING INTENTION AND BEHAVIORS
names(No_miceData[27:31])
res.pca3 <- prcomp(No_miceData[27:31],scale= TRUE)  
summary(res.pca3)

library("factoextra")
eig.val3 <- get_eigenvalue(res.pca3)
eig.val3

dimT3 <- c(1:5)
dimT3

#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT3, eig.val3$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")

#StreePlot
fviz_eig(res.pca3)

#Loading Score
fviz_pca_var(res.pca3,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#summary(No_miceData[27:31])
#plot(cor(No_miceData[27:31]))

# D. PERCEIVED BEHAVIOR CONTROL OVER PLASTIC BEHAVIOR INTENTION AND BEHAVIORAL CHANGE
names(No_miceData[32:48])
res.pca4 <- prcomp(No_miceData[32:48],scale= TRUE)  
summary(res.pca4)

library("factoextra")
eig.val4 <- get_eigenvalue(res.pca4)
eig.val4

dimT4 <- c(1:17)
dimT4

#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT4, eig.val4$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")

#StreePlot
fviz_eig(res.pca4)

#Loading Score
fviz_pca_var(res.pca4,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# E. Plastic Related Behaviour 
names(No_miceData[49:56])
res.pca5 <- prcomp(No_miceData[49:56],scale= TRUE)  
summary(res.pca5)

library("factoextra")
eig.val5 <- get_eigenvalue(res.pca5)
eig.val5

dimT5 <- c(1:8)
dimT5

#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT5, eig.val5$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")

#StreePlot
fviz_eig(res.pca5)
#Loading Score
fviz_pca_var(res.pca5,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# F Intention to change Behaviour 
names(No_miceData[57:60])
res.pca6 <- prcomp(No_miceData[57:60],scale= TRUE)  
summary(res.pca6)

library("factoextra")
eig.val6 <- get_eigenvalue(res.pca6)
eig.val6

dimT6 <- c(1:4)
dimT6

#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT6, eig.val6$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")

#StreePlot
fviz_eig(res.pca6)
#Loading Score
fviz_pca_var(res.pca6,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# G Communication related plastics 
names(No_miceData[61:66])
res.pca7 <- prcomp(No_miceData[61:66],scale= TRUE)  
summary(res.pca7)

library("factoextra")
eig.val7 <- get_eigenvalue(res.pca6)
eig.val7

dimT7 <- c(1:4)
dimT7

#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT7, eig.val7$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")

#StreePlot
fviz_eig(res.pca7)
#Loading Score
fviz_pca_var(res.pca7,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))



####Influence on gender on knowledge

Gender_Knowledge =No_miceData[,c("Gender","q1k","q2k","q3k","q4k","Q6K","Q7K","Q8K","Q9K")]

Gender_Knowledge_DA <- lda(Gender~q1k+q2k+q3k+q4k+Q6K+Q7K+Q8K+Q9K,data=Gender_Knowledge)
Gender_Knowledge_DA

#LDA preduction
lda.testing <- predict(Gender_Knowledge_DA)
#confusion matrix
accuracy <- table(lda.testing$class,No_miceData$Gender)
accuracy
sum(accuracy[row(accuracy) == col(accuracy)]) / sum(accuracy)

#FA
Gender_Knowledge_FA <- factanal(No_miceData[,c("q1k","q2k","q3k","q4k","Q6K","Q7K","Q8K","Q9K")], factors = 4, rotation = "promax")
Gender_Knowledge_FA

No_miceData[,c("q1k","q2k","q3k","q4k","Q6K","Q7K","Q8K","Q9K")]

#PCA with gender and knowledge
res.pca1 <- prcomp(No_miceData[,c("q1k","q2k","q3k","q4k","Q6K","Q7K","Q8K","Q9K")],scale= TRUE)  
summary(res.pca1)

library("factoextra")
eig.val1 <- get_eigenvalue(res.pca1)
eig.val1

dimT1 <- c(1:8)

#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT1, eig.val1$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")

#StreePlot
fviz_eig(res.pca1)

#Loading score
fviz_pca_var(res.pca1,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Fit the full model 
full.model <- lm(Gender ~., data = No_miceData[,c("Gender","q1k","q2k","q3k","q4k","Q6K","Q7K","Q8K","Q9K")])
#Stepwise regression
library()
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)

mastercopy3 <- No_miceData
mastercopy3$Gender[mastercopy3$Gender==2]=0
+
  #Logistic regression
  Reg_log_Gender_Know<- glm(Gender~ factor(q1k)+factor(q2k)+factor(q3k)+factor(q4k)+factor(Q6K)
                            +factor(Q7K)+factor(Q8K)+factor(Q9K), family=binomial, data=mastercopy3[,c("Gender","q1k","q2k","q3k","q4k","Q6K","Q7K","Q8K","Q9K")])
summary(Reg_log_Gender_Know)

pchisq(1110.5-1036.7, df = 982-963, lower.tail = FALSE)

Gender_q1k <- No_miceData[,c("Gender","q1k")] 
#CHECKING ANSWR OF GENDER VS Q1k
unique(Gender_q1k)
219+224

#Checking answer of gender vs Knowlegdge
table(No_miceData[,c("Gender","q1k")])
#Percentage for gender vs q1k
table(No_miceData[,c("Gender","q1k")])[1,]/sum(table(No_miceData[,c("Gender","q1k")])[1,])*100
table(No_miceData[,c("Gender","q1k")])[2,]/sum(table(No_miceData[,c("Gender","q1k")])[2,])*100

#Percentage for gender vs q2k
table(No_miceData[,c("Gender","q2k")])[1,]/sum(table(No_miceData[,c("Gender","q2k")])[1,])*100
table(No_miceData[,c("Gender","q2k")])[2,]/sum(table(No_miceData[,c("Gender","q2k")])[2,])*100

#Percentage for gender vs q3k
table(No_miceData[,c("Gender","q3k")])[1,]/sum(table(No_miceData[,c("Gender","q3k")])[1,])*100
table(No_miceData[,c("Gender","q3k")])[2,]/sum(table(No_miceData[,c("Gender","q3k")])[2,])*100

#Percentage for gender vs q4k
table(No_miceData[,c("Gender","q4k")])[1,]/sum(table(No_miceData[,c("Gender","q4k")])[1,])*100
table(No_miceData[,c("Gender","q4k")])[2,]/sum(table(No_miceData[,c("Gender","q4k")])[2,])*100

#Percentage for gender vs Q5
table(No_miceData[,c("Gender","Q5K1")])[1,]/sum(table(No_miceData[,c("Gender","Q5K1")])[1,])*100
table(No_miceData[,c("Gender","Q5K1")])[2,]/sum(table(No_miceData[,c("Gender","Q5K1")])[2,])*100

table(No_miceData[,c("Gender","Q5K2")])[1,]/sum(table(No_miceData[,c("Gender","Q5K2")])[1,])*100
table(No_miceData[,c("Gender","Q5K2")])[2,]/sum(table(No_miceData[,c("Gender","Q5K2")])[2,])*100

table(No_miceData[,c("Gender","Q5K3")])[1,]/sum(table(No_miceData[,c("Gender","Q5K3")])[1,])*100
table(No_miceData[,c("Gender","Q5K3")])[2,]/sum(table(No_miceData[,c("Gender","Q5K3")])[2,])*100

table(No_miceData[,c("Gender","Q5K4")])[1,]/sum(table(No_miceData[,c("Gender","Q5K4")])[1,])*100
table(No_miceData[,c("Gender","Q5K4")])[2,]/sum(table(No_miceData[,c("Gender","Q5K4")])[2,])*100

table(No_miceData[,c("Gender","Q5K5")])[1,]/sum(table(No_miceData[,c("Gender","Q5K5")])[1,])*100
table(No_miceData[,c("Gender","Q5K5")])[2,]/sum(table(No_miceData[,c("Gender","Q5K5")])[2,])*100

table(No_miceData[,c("Gender","Q5K6")])[1,]/sum(table(No_miceData[,c("Gender","Q5K6")])[1,])*100
table(No_miceData[,c("Gender","Q5K6")])[2,]/sum(table(No_miceData[,c("Gender","Q5K6")])[2,])*100


#Percentage for gender vs Q6K
table(No_miceData[,c("Gender","Q6K")])[1,]/sum(table(No_miceData[,c("Gender","Q6K")])[1,])*100
table(No_miceData[,c("Gender","Q6K")])[2,]/sum(table(No_miceData[,c("Gender","Q6K")])[2,])*100

#Percentage for gender vs Q7K
table(No_miceData[,c("Gender","Q7K")])[1,]/sum(table(No_miceData[,c("Gender","Q7K")])[1,])*100
table(No_miceData[,c("Gender","Q7K")])[2,]/sum(table(No_miceData[,c("Gender","Q7K")])[2,])*100

#Percentage for gender vs Q8K
table(No_miceData[,c("Gender","Q8K")])[1,]/sum(table(No_miceData[,c("Gender","Q8K")])[1,])*100
table(No_miceData[,c("Gender","Q8K")])[2,]/sum(table(No_miceData[,c("Gender","Q8K")])[2,])*100

#Percentage for gender vs Q9K
table(No_miceData[,c("Gender","Q9K")])[1,]/sum(table(No_miceData[,c("Gender","Q9K")])[1,])*100
table(No_miceData[,c("Gender","Q9K")])[2,]/sum(table(No_miceData[,c("Gender","Q9K")])[2,])*100

#Running PCA

# A. KNOWLEGDE
res.pca1 <- prcomp(No_miceData[,c("q1k","q2k","q3k","q4k","Q5K1","Q5K2","Q5K3","Q5K4","Q5K5","Q6K","Q7K","Q8K","Q9K")])  
res.pca1
summary(res.pca1)

library("factoextra")
eig.val1 <- get_eigenvalue(res.pca1)
eig.val1

dimT1 <- c(1:13)

#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT1, eig.val1$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")

#StreePlot
fviz_eig(res.pca1)

#Loading score
fviz_pca_var(res.pca1,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
#MOST variation is among Q1, Q8 and Q3
#There is strong corelation in Q3 and Q8
# A. KNOWLEGDE and age
res.pca1 <- prcomp(No_miceData[,c("Occupation","q1k","q2k","q3k","q4k","Q5K1","Q5K2","Q5K3","Q5K4","Q5K5","Q6K","Q7K","Q8K","Q9K")])  
res.pca1
summary(res.pca1)
library("factoextra")
eig.val1 <- get_eigenvalue(res.pca1)
eig.val1
dimT1 <- c(1:14)
#Plot the cumulative percentage variance accounted for versus the index of the Components 0
plot(dimT1, eig.val1$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")
#StreePlot
fviz_eig(res.pca1)
#Loading score
fviz_pca_var(res.pca1,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#Running DA
Gender_Knowledge_DA <- lda(Gender~q2k+q3k+Q6K+Q8K,data=No_miceData)
Gender_Knowledge_DA

#LDA preduction
lda.testing <- predict(Gender_Knowledge_DA)
#confusion matrix
accuracy <- table(lda.testing$class,No_miceData$Gender)
accuracy
sum(accuracy[row(accuracy) == col(accuracy)]) / sum(accuracy)

#Percentage for gender vs q1k
table(No_miceData[,c("Gender","q1k")])[1,]/sum(table(No_miceData[,c("Gender","q1k")])[1,])*100
table(No_miceData[,c("Gender","q1k")])[2,]/sum(table(No_miceData[,c("Gender","q1k")])[2,])*100

#Percentage for Education vs q1k
table(No_miceData[,c("Education","q1k")])[1,]/sum(table(No_miceData[,c("Education","q1k")])[1,])*100
table(No_miceData[,c("Education","q1k")])[2,]/sum(table(No_miceData[,c("Education","q1k")])[2,])*100
table(No_miceData[,c("Education","q1k")])[3,]/sum(table(No_miceData[,c("Education","q1k")])[3,])*100
table(No_miceData[,c("Income","q1k")])

#Percentage for income vs q1k
table(No_miceData[,c("Income","q1k")])[1,]/sum(table(No_miceData[,c("Income","q1k")])[1,])*100
table(No_miceData[,c("Income","q1k")])[2,]/sum(table(No_miceData[,c("Income","q1k")])[2,])*100
table(No_miceData[,c("Income","q1k")])[3,]/sum(table(No_miceData[,c("Income","q1k")])[3,])*100


#Running PCA on Attitude and preceptions
names(No_miceData[,c("Q10C1","Q10C2","Q10C3","Q10C4","Q10C5","Q10C6","Q10C7")])
res.pca2 <- prcomp(No_miceData[,c("Q10C1","Q10C2","Q10C3","Q10C4","Q10C5","Q10C6","Q10C7")])  
res.pca2
summary(res.pca2)
library("factoextra")
eig.val2 <- get_eigenvalue(res.pca2)
eig.val2
dimT2 <- c(1:8)
#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT2, eig.val2$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")
#StreePlot
fviz_eig(res.pca2)
#Loading score
fviz_pca_var(res.pca2,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
#there is strong corelation between C1 and C2
#there is strong corelation between C3 and C4
#there is strong corelation between C5 and C6 and C7


#Percentage for gender vs Q10C4
table(No_miceData[,c("Gender","Q10C4")])[1,]/sum(table(No_miceData[,c("Gender","Q10C4")])[1,])*100
table(No_miceData[,c("Gender","Q10C4")])[2,]/sum(table(No_miceData[,c("Gender","Q10C4")])[2,])*100

#Percentage for Education vs Q10C4
table(No_miceData[,c("Education","Q10C4")])[1,]/sum(table(No_miceData[,c("Education","Q10C4")])[1,])*100
table(No_miceData[,c("Education","Q10C4")])[2,]/sum(table(No_miceData[,c("Education","Q10C4")])[2,])*100
table(No_miceData[,c("Education","Q10C4")])[3,]/sum(table(No_miceData[,c("Education","Q10C4")])[3,])*100
table(No_miceData[,c("Income","q1k")])

#Percentage for income vs q1k
table(No_miceData[,c("Income","Q10C4")])[1,]/sum(table(No_miceData[,c("Income","Q10C4")])[1,])*100
table(No_miceData[,c("Income","Q10C4")])[2,]/sum(table(No_miceData[,c("Income","Q10C4")])[2,])*100
table(No_miceData[,c("Income","Q10C4")])[3,]/sum(table(No_miceData[,c("Income","Q10C4")])[3,])*100

table(No_miceData$Q10C1)      
table(No_miceData$Q10C2)

table(No_miceData$Q10C3)
table(No_miceData$Q10C4)
table(No_miceData$Q10C5)
table(No_miceData$Q10C6)
table(No_miceData$Q10C7)

# Fit the full model 
full.model <- lm(Gender ~., data = No_miceData[,c("Gender","Q10C5","Q10C6","Q10C7")])
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

resid(step.model)

#Running PCA on Attitude and preceptions
names(No_miceData[,c("Education","Q10C1","Q10C2","Q10C3","Q10C4","Q10C5","Q10C6","Q10C7")])
res.pca2 <- prcomp(No_miceData[,c("Gender","Q10C1","Q10C2","Q10C3","Q10C4","Q10C5","Q10C6","Q10C7")])  
res.pca2
summary(res.pca2)
library("factoextra")
eig.val2 <- get_eigenvalue(res.pca2)
eig.val2
dimT2 <- c(1:8)
#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT2, eig.val2$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")
#StreePlot
fviz_eig(res.pca2)
#Loading score
fviz_pca_var(res.pca2,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


#C. SOCIAL NORMS AFFECTING PLASTIC CHANGING INTENTION AND BEHAVIORS
res.pca2 <- prcomp(No_miceData[,c("Q11C1","Q11C2","Q11C3","Q11C4","Q11C5")])  
res.pca2
summary(res.pca2)
library("factoextra")
eig.val2 <- get_eigenvalue(res.pca2)
eig.val2
dimT2 <- c(1:5)
#Plot the cumulative percentage variance accounted for versus the index of the Components 
plot(dimT2, eig.val2$cumulative.variance.percent, ylab = "Commulative Variance",xlab = "Principal Components")
#StreePlot
fviz_eig(res.pca2)
#Loading score
fviz_pca_var(res.pca2,axes = c(1,2),col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

round(cor(No_miceData[,c("Q11C1","Q11C2","Q11C3","Q11C4","Q11C5")]),2)

cor(No_miceData$q3k,No_miceData$Q8K)


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
#Third model with 9 factors
factana <- fa(data_only_corelated_try3,nfactors =11)
fa.diagram(factana)

library(psych)
fa.diagram(fact_an_try1$loadings)

install.packages("GPArotation")
library(GPArotation)
factanatry1 <- fa(data,nfactors = 11)
fa.diagram(factanatry1)
#As the p value is very low so model is not good.

#keeping only correlated variables keeping cutoff 0.1
install.packages("caret")
library('caret')
data_corelated_try1 = findCorrelation(cor(data), cutoff=0.1)
data_corelated_try1
hc= sort(data_corelated_try1)
data_only_corelated_try1 = data[, c(hc)]
dim(data_only_corelated_try1)
#We are left with only 44 variables.

factana <- fa(data_only_corelated_try1,nfactors = 11)
fa.diagram(factana)

#keeping only correlated variables keeping cutoff 0.15
data_corelated_try2 = findCorrelation(cor(data), cutoff=0.15)
data_corelated_try2
hc= sort(data_corelated_try2)
data_only_corelated_try2 = data[, c(hc)]
dim(data_only_corelated_try2)
#We are left with only 39 variables.

factana <- fa(data_only_corelated_try2,nfactors = 11)
fa.diagram(factana)
#relations are not  good will try with more cutt off

#keeping only correlated variables keeping cutoff 0.2
data_corelated_try3 = findCorrelation(cor(data), cutoff=0.3)
data_corelated_try3
hc= sort(data_corelated_try3)
data_only_corelated_try3 = data[, c(hc)]
dim(data_only_corelated_try3)
#We are left with only 29 variables.

#First model with 11 factors
fact_ana <- factanal(data_only_corelated_try3, factors = 11)
fact_ana
factana <- fa(data_only_corelated_try3,nfactors =11)
fa.diagram(factana)

#Second model with 10 factors
fact_ana <- factanal(data_only_corelated_try3, factors = 10)
fact_ana
factana <- fa(data_only_corelated_try3,nfactors =10)
fa.diagram(factana)

#Third model with 9 factors
fact_ana <- factanal(data_only_corelated_try3, factors = 9)
fact_ana
factana <- fa(data_only_corelated_try3,nfactors =9)
fa.diagram(factana)

#Fourth model with 8 factors
fact_ana <- factanal(data_only_corelated_try3, factors = 8)
fact_ana
factana <- fa(data_only_corelated_try3,nfactors =8)
fa.diagram(factana)


HEAD
colnames(factana$loadings)
colnames(factana$loadings) <- c("RecBeh",
                                "MngRspb",
                                "Rspb",
                                "SocAfct",
                                "HltPrcp",
                                "EnvCons",
                                "EnvMntlst",
                                "PercBeh",
                                "Knldge")

# Chi Square Analysis

#CHiOne fo  gender
chisq.test(table(No_miceData$Gender,No_miceData$Q18I1),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q18I2),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q18I3),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q15C4),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q15C3),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q15C2),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q17P4),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q17P3),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q17P5),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q11C2),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q11C4),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q11C5),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q11C1),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q17P1),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q17P2),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q10C1),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q10C2),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q10C3),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q10C5),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q10C6),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q12C1),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q12C2),correct = FALSE)
chisq.test(table(No_miceData$Gender,No_miceData$Q5K4),correct = FALSE) 
chisq.test(table(No_miceData$Gender,No_miceData$Q5K5),correct = FALSE) 
chisq.test(table(No_miceData$Gender,No_miceData$Q5K1),correct = FALSE) 

#Chi Square with Education
chisq.test(table(No_miceData$Education,No_miceData$Q18I1),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q18I2),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q18I3),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q15C4),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q15C3),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q15C2),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q17P4),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q17P3),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q17P5),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q11C2),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q11C4),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q11C5),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q11C1),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q17P1),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q17P2),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q10C1),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q10C2),correct = FALSE) 
chisq.test(table(No_miceData$Education,No_miceData$Q10C3),correct = FALSE)
chisq.test(table(No_miceData$Education,No_miceData$Q10C5),correct = FALSE)
chisq.test(table(No_miceData$Education,No_miceData$Q10C6),correct = FALSE)
chisq.test(table(No_miceData$Education,No_miceData$Q12C1),correct = FALSE)
chisq.test(table(No_miceData$Education,No_miceData$Q12C2),correct = FALSE)
chisq.test(table(No_miceData$Education,No_miceData$Q5K4),correct = FALSE)  
chisq.test(table(No_miceData$Education,No_miceData$Q5K5),correct = FALSE)  
chisq.test(table(No_miceData$Education,No_miceData$Q5K1),correct = FALSE) 

#Chi Square with occupation
chisq.test(table(No_miceData$Occupation,No_miceData$Q18I1),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q18I2),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q18I3),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q15C4),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q15C3),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q15C2),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q17P4),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q17P3),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q17P5),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q11C2),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q11C4),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q11C5),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q11C1),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q17P1),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q17P2),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q10C1),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q10C2),correct = FALSE) 
chisq.test(table(No_miceData$Occupation,No_miceData$Q10C3),correct = FALSE)
chisq.test(table(No_miceData$Occupation,No_miceData$Q10C5),correct = FALSE)
chisq.test(table(No_miceData$Occupation,No_miceData$Q10C6),correct = FALSE)
chisq.test(table(No_miceData$Occupation,No_miceData$Q12C1),correct = FALSE)
chisq.test(table(No_miceData$Occupation,No_miceData$Q12C2),correct = FALSE)
chisq.test(table(No_miceData$Occupation,No_miceData$Q5K4),correct = FALSE)  
chisq.test(table(No_miceData$Occupation,No_miceData$Q5K5),correct = FALSE)  
chisq.test(table(No_miceData$Occupation,No_miceData$Q5K1),correct = FALSE) 

#Chi Square with Income
chisq.test(table(No_miceData$Income,No_miceData$Q18I1),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q18I2),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q18I3),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q15C4),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q15C3),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q15C2),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q17P4),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q17P3),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q17P5),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q11C2),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q11C4),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q11C5),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q11C1),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q17P1),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q17P2),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q10C1),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q10C2),correct = FALSE) 
chisq.test(table(No_miceData$Income,No_miceData$Q10C3),correct = FALSE)
chisq.test(table(No_miceData$Income,No_miceData$Q10C5),correct = FALSE)
chisq.test(table(No_miceData$Income,No_miceData$Q10C6),correct = FALSE)
chisq.test(table(No_miceData$Income,No_miceData$Q12C1),correct = FALSE)
chisq.test(table(No_miceData$Income,No_miceData$Q12C2),correct = FALSE)
chisq.test(table(No_miceData$Income,No_miceData$Q5K4),correct = FALSE)  
chisq.test(table(No_miceData$Income,No_miceData$Q5K5),correct = FALSE)  
chisq.test(table(No_miceData$Income,No_miceData$Q5K1),correct = FALSE)
