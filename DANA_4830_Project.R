# Checking Accuracy of Data

#Import the data and make copy of data
master <- read.csv("Data-screening-1 (1).csv")


mastercopy <- master

#Checking names of columns and remove all those column which are not in appendix or not relative to study.
names(mastercopy)
dim(mastercopy)
#dropping unnecessory rows
mastercopy <- mastercopy[,-c(1:16)]
names(mastercopy)

#dimension of the data set
dim(mastercopy)

#Now removing all the duplicate values if it has. By using package "dplyr".
library("dplyr")
mastercopy<- distinct(mastercopy)
dim(mastercopy)

##As we see there are 7 duplicate values in dataset which we removed.

### Start checking accuarcy for the columns containing DEMOGRAPHIC INFORMATION
#To see unique values
lapply(mastercopy, unique)[1:5]

#Except Education and Occupation, all other columns have wrong data entry. We will correct it acoording to the appendix and remove all the values which are out of range of appendix. Also the children below 5 or above 100 is for sure is wrong we need to remove that one.
#Gender
mastercopy$Gender <- factor(mastercopy$Gender, levels = c(1,2))
#Age
mastercopy$Age[mastercopy$Age=="16 years-old"] <- "16"
#mastercopy$Age[mastercopy$Age=="1970"] <- "16"
#mastercopy$Age[mastercopy$Age=="16 years-old"] <- "16"

library("stringr")
mastercopy$Age <-str_replace_all(mastercopy$Age, "[abcdefgfhijklmanopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]", NA_character_)
mastercopy$Age <-strtoi(mastercopy$Age)
#It makes no sense if the person age more than 100 or less tha 5 contribute in survey.
mastercopy$Age[(mastercopy["Age"] < 12)] = NA
mastercopy$Age[(mastercopy["Age"] >100)] =NA 
#Income
#as one enntry is "30000" it falls under the category of 3 so to change it.
mastercopy$Income[mastercopy$Income=="30000"] <- "3"
str(mastercopy)
mastercopy$Income <-strtoi(mastercopy$Income)
dim(mastercopy)

### Start checking accuarcy for the columns containing KNOWLEDGE.
lapply(mastercopy, unique)[6:20]
#We need to remove all the values which are out of range. But as only Q6 has some values out of range. So we will replace those values with Null values.
mastercopy$Q6K <- factor(mastercopy$Q6K, levels = c(1,2))

### Start checking accuarcy for the columns containing ATTITUDE AND PERCEPTION.
lapply(mastercopy,unique)[21:27]
#So this part of data is accuarte and according to the appendix.

### Start checking accuarcy for the columns containing SOCIAL NORMS AFFECTING PLASTIC CHANGING INTENTION AND BEHAVIORS.
lapply(mastercopy, unique)[28:32]

#As the question 11 has only three option to select, so we will remove other values,
mastercopy$Q11C1 <- factor(mastercopy$Q11C1, levels = c(1,2,3))
mastercopy$Q11C2 <- factor(mastercopy$Q11C2, levels = c(1,2,3))
mastercopy$Q11C3 <- factor(mastercopy$Q11C3, levels = c(1,2,3))
mastercopy$Q11C4 <- factor(mastercopy$Q11C4, levels = c(1,2,3))
mastercopy$Q11C5 <- factor(mastercopy$Q11C5, levels = c(1,2,3))

### Start checking accuarcy for the columns containing PERCEIVED BEHAVIOR CONTROL OVER PLASTIC BEHAVIOR INTENTION AND BEHAVIORAL CHANGE
lapply(mastercopy, unique)[33:52]

#As in Q12, option 8 is not there. Now, we will remove that column. 
library("dplyr")
mastercopy <- select(mastercopy, -Q12C8)

#Now update other column as needed according to appedix.

mastercopy$Q12C1 <- factor(mastercopy$Q12C1, levels = c(1,2,3,4,5,6))
mastercopy$Q12C2 <- factor(mastercopy$Q12C2, levels = c(1,2,3,4,5,6))
mastercopy$Q12C3 <- factor(mastercopy$Q12C3, levels = c(1,2,3,4,5,6))
mastercopy$Q12C4 <- factor(mastercopy$Q12C4, levels = c(1,2,3,4,5,6))
mastercopy$Q12C5 <- factor(mastercopy$Q12C5, levels = c(1,2,3,4,5,6))
mastercopy$Q12C6 <- factor(mastercopy$Q12C6, levels = c(1,2,3,4,5,6))
mastercopy$Q13C1 <- factor(mastercopy$Q13C1, levels = c(1,2,3))
mastercopy$Q13C2 <- factor(mastercopy$Q13C2, levels = c(1,2,3))
mastercopy$Q13C3 <- factor(mastercopy$Q13C3, levels = c(1,2,3))
mastercopy$Q13C4 <- factor(mastercopy$Q13C4, levels = c(1,2,3))
mastercopy$Q13C5 <- factor(mastercopy$Q13C5, levels = c(1,2,3))
mastercopy$Q13C6 <- factor(mastercopy$Q13C6, levels = c(1,2,3))



### Start checking accuarcy for the columns containing PLASTIC - RELATED BEHAVIORS.
#We check for the unique values.

lapply(mastercopy, unique)[52:61]

#Data is accurate.

### Start checking accuarcy for the columns containing INTENTIONS TO CHANGE BEHAVIORS.

lapply(mastercopy, unique)[62:65]

#So, We can see that 5th sub question in Q18 is not recorded. And all other data entries are have range from 1 to 5 so we need to remove the 6 level.
mastercopy$Q18I1 <- factor(mastercopy$Q18I1, levels = c(1,2,3,4,5))
mastercopy$Q18I2 <- factor(mastercopy$Q18I2, levels = c(1,2,3,4,5))
mastercopy$Q18I3 <- factor(mastercopy$Q18I3, levels = c(1,2,3,4,5))
mastercopy$Q18I4 <- factor(mastercopy$Q18I4, levels = c(1,2,3,4,5))

### Start checking accuarcy for the columns containing COMMUNICATIONS RELATED PLASTICS.

lapply(mastercopy,unique)[66:71]

#As the accuarcy is correct But last part is not recorded. Also, we need to update the columns names.

names(mastercopy)[66:71] <- c("Q19C1","Q19C2","Q19C3","Q19C4","Q19c5","Q19c6")

dim((mastercopy))
#Changing type of 12 into numeric as we need to change the its vale
mastercopy$Q12C1<- as.numeric(mastercopy$Q12C1)
mastercopy$Q12C2<- as.numeric(mastercopy$Q12C2)
mastercopy$Q12C3<- as.numeric(mastercopy$Q12C3)
mastercopy$Q12C4<- as.numeric(mastercopy$Q12C4)
mastercopy$Q12C5<- as.numeric(mastercopy$Q12C5)
mastercopy$Q12C6<- as.numeric(mastercopy$Q12C6)

#Changing Q12 values to 1
mastercopy$Q12C1[mastercopy$Q12C1 > 1] = 1
mastercopy$Q12C2[mastercopy$Q12C2 > 1] = 1
mastercopy$Q12C3[mastercopy$Q12C3 > 1] = 1
mastercopy$Q12C4[mastercopy$Q12C4 > 1] = 1
mastercopy$Q12C5[mastercopy$Q12C5 > 1] = 1
mastercopy$Q12C6[mastercopy$Q12C6 > 1] = 1

## Accuracy done.

# Missing Values
#Lets have a look on the missing values in our data.
mastercopy1<-mastercopy
mastercopy1<-read.csv("mastercopy1.csv",stringsAsFactors = FALSE)
library("naniar")
vis_miss(mastercopy1)
dim(mastercopy1)

# To delete the columns which are having more than 90% DAta 
# To delete the rows which are having more than 50% data
mastercopy1<-mastercopy1[which(rowMeans(!is.na(mastercopy1)) > 0.5), ]
mastercopy1<-mastercopy1[,which(colMeans(!is.na(mastercopy1)) > 0.1)]
vis_miss(mastercopy1)
dim(mastercopy1)

  #to fill zero in multiple choice questions
  #We Start with Q5
  names(mastercopy1[10:15])
  mastercopy1[10:15][is.na(mastercopy1[10:15])] <- 0
  
  #with Q12
  lapply(mastercopy1,names)[33:37]
  mastercopy1[33:37][is.na(mastercopy1[33:37])] <- 0
  
  #with Q13
  names(mastercopy1[38:42])
  mastercopy1[38:42][is.na(mastercopy1[38:42])] <- 0
  mastercopy1[38:42][mastercopy1[38:42] > 1] <- 1
  
  #with Q15
  names(mastercopy1)[44:48]
  mastercopy1[44:48][is.na(mastercopy1[44:48])] <- 0
  
  #with Q16
  names(mastercopy1)[49:51]
  mastercopy1[49:51][is.na(mastercopy1[49:51])] <- 0
  
  #with Q19
  names(mastercopy1)[61:66]
  mastercopy1[61:66][is.na(mastercopy1[61:66])] <- 0
  dim(mastercopy1)
  vis_miss(mastercopy1)

  

  # Creating a data set by just removing null values. 
noMiceDataset <- na.omit(mastercopy1)
dim(noMiceDataset)
vis_miss(noMiceDataset)  
#Finally import the noMiceData  set
write.csv(noMiceDataset,"final_No_mice.csv", row.names = FALSE)
#Summary of the data
summary(mastercopy1)
  

#Q19 descriptive analysis
View(mastercopy1)
table(mastercopy1$Q19C1)
table(mastercopy1$Q19C2)
table(mastercopy1$Q19C3)
table(mastercopy1$Q19C4)
table(mastercopy1$Q19c5)
table(mastercopy1$Q19c6)



#Applying Mice to fill null values
install.packages("mice")
library(mice)
nomissdf= mice(mastercopy1)
df1=complete(nomissdf,1)
summary(df1)
library(naniar)
vis_miss(df1)

mastercopy2 <- df1

#Finally import the Final data set
write.csv(mastercopy2,"final.csv", row.names = FALSE)


##Make Sure Q12,Q13 and Q19 are of making orders
#it is priority based. For Q19 we have 6 options
#19c1 to 19c6 and first row has values 2,3,4,5,1,6 in order
#or priorities for the first person.
names(mastercopy2[1:5])


#PCA
res.pca0 <- prcomp(mastercopy2[1:5],scale= TRUE)  
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
res.pca1 <- prcomp(mastercopy2[6:19],scale= TRUE)  
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
names(mastercopy2[20:26])
res.pca2 <- prcomp(mastercopy2[20:26],scale= TRUE)  
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
names(mastercopy2[27:31])
res.pca3 <- prcomp(mastercopy2[27:31],scale= TRUE)  
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

#summary(mastercopy2[27:31])
#plot(cor(mastercopy2[27:31]))

# D. PERCEIVED BEHAVIOR CONTROL OVER PLASTIC BEHAVIOR INTENTION AND BEHAVIORAL CHANGE
names(mastercopy2[32:48])
res.pca4 <- prcomp(mastercopy2[32:48],scale= TRUE)  
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
names(mastercopy2[49:56])
res.pca5 <- prcomp(mastercopy2[49:56],scale= TRUE)  
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
names(mastercopy2[57:60])
res.pca6 <- prcomp(mastercopy2[57:60],scale= TRUE)  
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
####Influence on gender on knowledge

#Checking answer of gender vs Knowledge
table(mastercopy2[,c("Gender","q1k")])
#Percentage for gender vs q1k
table(mastercopy2[,c("Gender","q1k")])[1,]/sum(table(mastercopy2[,c("Gender","q1k")])[1,])*100
table(mastercopy2[,c("Gender","q1k")])[2,]/sum(table(mastercopy2[,c("Gender","q1k")])[2,])*100

#Percentage for gender vs q2k
table(mastercopy2[,c("Gender","q2k")])[1,]/sum(table(mastercopy2[,c("Gender","q2k")])[1,])*100
table(mastercopy2[,c("Gender","q2k")])[2,]/sum(table(mastercopy2[,c("Gender","q2k")])[2,])*100

#Percentage for gender vs q3k
table(mastercopy2[,c("Gender","q3k")])[1,]/sum(table(mastercopy2[,c("Gender","q3k")])[1,])*100
table(mastercopy2[,c("Gender","q3k")])[2,]/sum(table(mastercopy2[,c("Gender","q3k")])[2,])*100

#Percentage for gender vs q4k
table(mastercopy2[,c("Gender","q4k")])[1,]/sum(table(mastercopy2[,c("Gender","q4k")])[1,])*100
table(mastercopy2[,c("Gender","q4k")])[2,]/sum(table(mastercopy2[,c("Gender","q4k")])[2,])*100

#Percentage for gender vs Q5
table(mastercopy2[,c("Gender","Q5K1")])[1,]/sum(table(mastercopy2[,c("Gender","Q5K1")])[1,])*100
table(mastercopy2[,c("Gender","Q5K1")])[2,]/sum(table(mastercopy2[,c("Gender","Q5K1")])[2,])*100

table(mastercopy2[,c("Gender","Q5K2")])[1,]/sum(table(mastercopy2[,c("Gender","Q5K2")])[1,])*100
table(mastercopy2[,c("Gender","Q5K2")])[2,]/sum(table(mastercopy2[,c("Gender","Q5K2")])[2,])*100

table(mastercopy2[,c("Gender","Q5K3")])[1,]/sum(table(mastercopy2[,c("Gender","Q5K3")])[1,])*100
table(mastercopy2[,c("Gender","Q5K3")])[2,]/sum(table(mastercopy2[,c("Gender","Q5K3")])[2,])*100

table(mastercopy2[,c("Gender","Q5K4")])[1,]/sum(table(mastercopy2[,c("Gender","Q5K4")])[1,])*100
table(mastercopy2[,c("Gender","Q5K4")])[2,]/sum(table(mastercopy2[,c("Gender","Q5K4")])[2,])*100

table(mastercopy2[,c("Gender","Q5K5")])[1,]/sum(table(mastercopy2[,c("Gender","Q5K5")])[1,])*100
table(mastercopy2[,c("Gender","Q5K5")])[2,]/sum(table(mastercopy2[,c("Gender","Q5K5")])[2,])*100

table(mastercopy2[,c("Gender","Q5K6")])[1,]/sum(table(mastercopy2[,c("Gender","Q5K6")])[1,])*100
table(mastercopy2[,c("Gender","Q5K6")])[2,]/sum(table(mastercopy2[,c("Gender","Q5K6")])[2,])*100


#Percentage for gender vs Q6K
table(mastercopy2[,c("Gender","Q6K")])[1,]/sum(table(mastercopy2[,c("Gender","Q6K")])[1,])*100
table(mastercopy2[,c("Gender","Q6K")])[2,]/sum(table(mastercopy2[,c("Gender","Q6K")])[2,])*100

#Percentage for gender vs Q7K
table(mastercopy2[,c("Gender","Q7K")])[1,]/sum(table(mastercopy2[,c("Gender","Q7K")])[1,])*100
table(mastercopy2[,c("Gender","Q7K")])[2,]/sum(table(mastercopy2[,c("Gender","Q7K")])[2,])*100

#Percentage for gender vs Q8K
table(mastercopy2[,c("Gender","Q8K")])[1,]/sum(table(mastercopy2[,c("Gender","Q8K")])[1,])*100
table(mastercopy2[,c("Gender","Q8K")])[2,]/sum(table(mastercopy2[,c("Gender","Q8K")])[2,])*100

#Percentage for gender vs Q9K
table(mastercopy2[,c("Gender","Q9K")])[1,]/sum(table(mastercopy2[,c("Gender","Q9K")])[1,])*100
table(mastercopy2[,c("Gender","Q9K")])[2,]/sum(table(mastercopy2[,c("Gender","Q9K")])[2,])*100


table(mastercopy2[,c("Gender","Q10C1")])[1,]/sum(table(mastercopy2[,c("Gender","Q7K")])[1,])*100
table(mastercopy2[,c("Gender","Q10C1")])[2,]/sum(table(mastercopy2[,c("Gender","Q7K")])[2,])*100


#Running DA
Gender_Knowledge_DA <- lda(Gender~q2k+q3k+Q6K,data=mastercopy2)
Gender_Knowledge_DA

#LDA prediction
lda.testing <- predict(Gender_Knowledge_DA)
#confusion matrix
accuracy <- table(lda.testing$class,mastercopy2$Gender)
accuracy
sum(accuracy[row(accuracy) == col(accuracy)]) / sum(accuracy)

#Percentage for gender vs q1k
table(mastercopy2[,c("Gender","q1k")])[1,]/sum(table(mastercopy2[,c("Gender","q1k")])[1,])*100
table(mastercopy2[,c("Gender","q1k")])[2,]/sum(table(mastercopy2[,c("Gender","q1k")])[2,])*100

#Percentage for Education vs q1k
table(mastercopy2[,c("Education","q1k")])[1,]/sum(table(mastercopy2[,c("Education","q1k")])[1,])*100
table(mastercopy2[,c("Education","q1k")])[2,]/sum(table(mastercopy2[,c("Education","q1k")])[2,])*100

table(mastercopy2[,c("Education","q1k")])[3,]/sum(table(mastercopy2[,c("Education","q1k")])[3,])*100
table(mastercopy2[,c("Income","q1k")])

#Percentage for income vs q1k
table(mastercopy2[,c("Income","q1k")])[1,]/sum(table(mastercopy2[,c("Income","q1k")])[1,])*100
table(mastercopy2[,c("Income","q1k")])[2,]/sum(table(mastercopy2[,c("Income","q1k")])[2,])*100
table(mastercopy2[,c("Income","q1k")])[3,]/sum(table(mastercopy2[,c("Income","q1k")])[3,])*100


#Running PCA on Attitude and preceptions
names(mastercopy2[,c("Q10C1","Q10C2","Q10C3","Q10C4","Q10C5","Q10C6","Q10C7")])
res.pca2 <- prcomp(mastercopy2[,c("Q10C1","Q10C2","Q10C3","Q10C4","Q10C5","Q10C6","Q10C7")])  
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
table(mastercopy2[,c("Gender","Q10C4")])[1,]/sum(table(mastercopy2[,c("Gender","Q10C4")])[1,])*100
table(mastercopy2[,c("Gender","Q10C4")])[2,]/sum(table(mastercopy2[,c("Gender","Q10C4")])[2,])*100

#Percentage for Education vs Q10C4
table(mastercopy2[,c("Education","Q10C4")])[1,]/sum(table(mastercopy2[,c("Education","Q10C4")])[1,])*100
table(mastercopy2[,c("Education","Q10C4")])[2,]/sum(table(mastercopy2[,c("Education","Q10C4")])[2,])*100
table(mastercopy2[,c("Education","Q10C4")])[3,]/sum(table(mastercopy2[,c("Education","Q10C4")])[3,])*100
table(mastercopy2[,c("Income","q1k")])

#Percentage for income vs q1k
table(mastercopy2[,c("Income","Q10C4")])[1,]/sum(table(mastercopy2[,c("Income","Q10C4")])[1,])*100
table(mastercopy2[,c("Income","Q10C4")])[2,]/sum(table(mastercopy2[,c("Income","Q10C4")])[2,])*100
table(mastercopy2[,c("Income","Q10C4")])[3,]/sum(table(mastercopy2[,c("Income","Q10C4")])[3,])*100

table(mastercopy2$Q10C1)      
table(mastercopy2$Q10C2)

table(mastercopy2$Q10C3)
table(mastercopy2$Q10C4)
table(mastercopy2$Q10C5)
table(mastercopy2$Q10C6)
table(mastercopy2$Q10C7)

table(mastercopy2$Q10C7)


# Fit the full model 
full.model <- lm(Gender ~., data = mastercopy2[,c("Gender","Q10C5","Q10C6","Q10C7")])
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

#As per knowledge and reserach Q19 and demographic donot contribute much so we remove those from columns
#Try Factor Analysis

##Factor Analysis

#Data without question 19
data_without19 <- mastercopy2[,-(61:66)]
#we delete col 19 as it is not that much important

#Now we also dont need demographic as per requirement of project
data<-data_without19[,-(1:5)]

#DEcide number of initial factors
library(psych)
decidefactor <- fa.parallel(data,fm ='ml', fa = 'fa')
#According to parallel analysis we have 11 factors

#As we see by the parallel analysis number of factor should be 11.

#Try 1

factana11 <- factanal(data,factors =11)
factana11
fact_ana11 <- fa(data,nfactors =11)
fa.diagram(fact_ana11)
summary(fact_ana11)


library(GPArotation)
factanatry1 <- fa(data,nfactors = 11)
library(psych)
fa.diagram(factana1)
colnames(factanatry1$loadings)

colnames(factanatry1$loadings)=c("PlasBeh",
                        "ReduInt",
                        "SysResp",
                        "SocAffect"
                        ,"NatrConc",
                        "HealConc"
                        ,"CondBeh"
                        ,"IngEff"
                        ,"BehChan"
                        ,"ConEfft",
                        "Diff")
#As the p value is very low so model is not good.


library('caret')
#<<<<<<< HEAD



#Model Donot improve much So we need to increase the cutoff

#keeping only correlated variables keeping cutoff 0.2
data_corelated_try2 = findCorrelation(cor(data), cutoff=0.2)
data_corelated_try2
hc= sort(data_corelated_try2)
data_only_corelated_try2 = data[, c(hc)]
dim(data_only_corelated_try2)
#We are left with only 31 variables.

#Decide number of factors
#DEcide number of initial factors
library(psych)
decidefactor <- fa.parallel(data_only_corelated_try2,fm ='ml', fa = 'fa')
#According to parallel analysis we have 10 factors


factana11_0.2_1 <- factanal(data_only_corelated_try2, factors = 10)
factana11_0.2_1
fact_ana11_0.2_1 <- fa(data_only_corelated_try2,nfactors =10)
summary(fact_ana11_0.2_1)
fa.diagram(fact_ana11_0.2_1)

factana11_0.2_2 <- factanal(data_only_corelated_try2, factors = 9)
factana11_0.2_2
fact_ana11_0.2_2 <- fa(data_only_corelated_try2,nfactors =9)
summary(fact_ana11_0.2_2)
fa.diagram(fact_ana11_0.2_2)

factana11_0.2_3 <- factanal(data_only_corelated_try2, factors = 8)
factana11_0.2_3
fact_ana11_0.2_3 <- fa(data_only_corelated_try2,nfactors =8)
summary(fact_ana11_0.2_3)
fa.diagram(fact_ana11_0.2_3)

factana11_0.2_4 <- factanal(data_only_corelated_try2, factors = 7)
factana11_0.2_4
fact_ana11_0.2_4 <- fa(data_only_corelated_try2,nfactors =7)
summary(fact_ana11_0.2_4)
fa.diagram(fact_ana11_0.2_4)


#<<<<<<< HEAD


colnames(fa_psych$loadings) <- c("Behaviour[Reduce]", "Intention[Reduce]","Personal Accountability[Reduce]",
                                 "Health Concern [Att.]", "Social Conditional Intention",
                                 "Impediments","Environemental Concern [Att.]")

#Model did not Improves but we will try with cut off 0.3

#keeping only correlated variables keeping cutoff 0.3
data_corelated_try3 = findCorrelation(cor(data), cutoff=0.3)
data_corelated_try3
hc= sort(data_corelated_try3)
data_only_corelated_try3 = data[, c(hc)]
dim(data_only_corelated_try3)
#We are left with only 29 variables.

#Decide number of factors
#DEcide number of initial factors
library(psych)
decidefactor <- fa.parallel(data_only_corelated_try3,fm ='ml', fa = 'fa')
#According to parallel analysis we have 9 factors

#First model with 9 factors
factana11_1 <- factanal(data_only_corelated_try3, factors = 9)
factana11_1
fact_ana11_1 <- fa(data_only_corelated_try3,nfactors =9)
summary(fact_ana11_1)
fa.diagram(fact_ana11_1)

#Second model with 8 factors
factana11_2 <- factanal(data_only_corelated_try3, factors = 8)
factana11_2
fact_ana11_2 <- fa(data_only_corelated_try3,nfactors =8)
summary(fact_ana11_2)
fa.diagram(fact_ana11_2)

#Third model with 7 factors
factana11_3 <- factanal(data_only_corelated_try3, factors = 7)
factana11_3
fact_ana11_3 <- fa(data_only_corelated_try3,nfactors =7)
summary(fact_ana11_3)
fa.diagram(fact_ana11_3)


#Forth model with 6 factors
factana11_4 <- factanal(data_only_corelated_try3, factors = 6)
factana11_4
fact_ana11_4 <- fa(data_only_corelated_try3,nfactors =6)
summary(fact_ana11_4)
fa.diagram(fact_ana11_4)

#Fifth model with 5 factors
factana11_5 <- factanal(data_only_corelated_try3, factors = 5)
factana11_5
fact_ana11_5 <- fa(data_only_corelated_try3,nfactors =5)
summary(fact_ana11_5)
fa.diagram(fact_ana11_5)


#On the Dataset without mice

Nomice <- na.omit(mastercopy1)

#Select Only corelated columns with cut of of 0.3
#keeping only correlated variables keeping cutoff 0.3
NomiceCor = findCorrelation(cor(Nomice), cutoff=0.3)
hc= sort(NomiceCor)
Nomicecor = Nomice[, c(hc)]
dim(Nomicecor)
#We are left with only 29 variables.

#Decide number of factors
#DEcide number of initial factors
library(psych)
decidefactor <- fa.parallel(Nomicecor,fm ='ml', fa = 'fa')
#According to parallel analysis we have 9 factors
#First model with 10 factors
factnomice10 <- factanal(Nomicecor, factors = 10)
factnomice10
factnomice_10 <- fa(Nomicecor,nfactors =10)
summary(factnomice_10)
fa.diagram(factnomice_10)

#Second model with 9 factors
factnomice9 <- factanal(Nomicecor, factors = 9)
factnomice9
factnomice_9 <- fa(Nomicecor,nfactors =9)
summary(factnomice_9)
fa.diagram(factnomice_9)

#Third model with 8 factors
factnomice8 <- factanal(Nomicecor, factors = 8)
factnomice8
factnomice_8 <- fa(Nomicecor,nfactors =8)
summary(factnomice_8)
fa.diagram(factnomice_8)


#Forth model with 7 factors
factnomice7 <- factanal(Nomicecor, factors = 7)
factnomice7
factnomice_7 <- fa(Nomicecor,nfactors =7)
summary(factnomice_7)
fa.diagram(factnomice_7)

#Fifth model with 6 factors
factnomice6 <- factanal(Nomicecor, factors = 6)
factnomice6
factnomice_6 <- fa(Nomicecor,nfactors =6)
summary(factnomice_6)
fa.diagram(factnomice_6)

######LDA PREDICTING GENDER ON THE BASIS OF THEIR PLASTIC RELATED BEHAVIOUR

Gender_beh =mastercopy2[,c("Gender","Q17P1","Q17P2","Q17P3","Q17P4","Q17P5")]
library(MASS)
Gender_beh_DA <- lda(Gender~Q17P1+Q17P2+Q17P3+Q17P4+Q17P5,data=Gender_Knowledge)
Gender_beh_DA

#LDA preduction
lda.testing <- predict(Gender_beh_DA)
#confusion matrix
accuracy <- table(lda.testing$class,Gender_beh$Gender)
accuracy
sum(accuracy[row(accuracy) == col(accuracy)]) / sum(accuracy)

#Regression Try

# Fit the full model 
full.model <- lm(Gender ~., data = Gender_beh)
summary(full.model)
#Stepwise regression
library(MASS)
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)
#CAnt do regression as it has very low p value.



# Chi Square Analysis

#CHiOne fo  gender
chisq.test(table(mastercopy2$Gender,mastercopy2$Q18I1),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q18I2),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q18I3),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q15C4),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q15C3),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q15C2),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q17P4),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q17P3),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q17P5),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q11C2),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q11C4),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q11C5),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q11C1),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q17P1),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q17P2),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q10C1),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q10C2),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q10C3),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q10C5),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q10C6),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q12C1),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q12C2),correct = FALSE)
chisq.test(table(mastercopy2$Gender,mastercopy2$Q5K4),correct = FALSE) 
chisq.test(table(mastercopy2$Gender,mastercopy2$Q5K5),correct = FALSE) 
chisq.test(table(mastercopy2$Gender,mastercopy2$Q5K1),correct = FALSE) 

#Chi Square with Education
chisq.test(table(mastercopy2$Education,mastercopy2$Q18I1),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q18I2),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q18I3),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q15C4),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q15C3),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q15C2),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q17P4),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q17P3),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q17P5),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q11C2),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q11C4),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q11C5),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q11C1),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q17P1),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q17P2),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q10C1),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q10C2),correct = FALSE) 
chisq.test(table(mastercopy2$Education,mastercopy2$Q10C3),correct = FALSE)
chisq.test(table(mastercopy2$Education,mastercopy2$Q10C5),correct = FALSE)
chisq.test(table(mastercopy2$Education,mastercopy2$Q10C6),correct = FALSE)
chisq.test(table(mastercopy2$Education,mastercopy2$Q12C1),correct = FALSE)
chisq.test(table(mastercopy2$Education,mastercopy2$Q12C2),correct = FALSE)
chisq.test(table(mastercopy2$Education,mastercopy2$Q5K4),correct = FALSE)  
chisq.test(table(mastercopy2$Education,mastercopy2$Q5K5),correct = FALSE)  
chisq.test(table(mastercopy2$Education,mastercopy2$Q5K1),correct = FALSE) 

#Chi Square with occupation
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q18I1),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q18I2),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q18I3),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q15C4),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q15C3),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q15C2),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q17P4),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q17P3),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q17P5),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q11C2),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q11C4),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q11C5),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q11C1),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q17P1),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q17P2),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q10C1),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q10C2),correct = FALSE) 
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q10C3),correct = FALSE)
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q10C5),correct = FALSE)
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q10C6),correct = FALSE)
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q12C1),correct = FALSE)
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q12C2),correct = FALSE)
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q5K4),correct = FALSE)  
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q5K5),correct = FALSE)  
chisq.test(table(mastercopy2$Occupation,mastercopy2$Q5K1),correct = FALSE) 

#Chi Square with Income
chisq.test(table(mastercopy2$Income,mastercopy2$Q18I1),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q18I2),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q18I3),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q15C4),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q15C3),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q15C2),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q17P4),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q17P3),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q17P5),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q11C2),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q11C4),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q11C5),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q11C1),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q17P1),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q17P2),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q10C1),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q10C2),correct = FALSE) 
chisq.test(table(mastercopy2$Income,mastercopy2$Q10C3),correct = FALSE)
chisq.test(table(mastercopy2$Income,mastercopy2$Q10C5),correct = FALSE)
chisq.test(table(mastercopy2$Income,mastercopy2$Q10C6),correct = FALSE)
chisq.test(table(mastercopy2$Income,mastercopy2$Q12C1),correct = FALSE)
chisq.test(table(mastercopy2$Income,mastercopy2$Q12C2),correct = FALSE)
chisq.test(table(mastercopy2$Income,mastercopy2$Q5K4),correct = FALSE)  
chisq.test(table(mastercopy2$Income,mastercopy2$Q5K5),correct = FALSE)  
chisq.test(table(mastercopy2$Income,mastercopy2$Q5K1),correct = FALSE) 




#####################################################################################################################
#############################################################################################################################
View(data_only_corelated_try3)
data_only_corelated_try3
#Corelation Short method
cordata <-cor(data_only_corelated_try3)
library(corrplot)
corrplot(cordata, method="number")
#Corelation
cormat <- round(cor(data_only_corelated_try3),2)
head(cormat)
install.packages("reshape2")
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)
ggheatmap +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


