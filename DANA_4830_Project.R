# Checking Accuracy of Data

#Import the data and make copy of data
master <- read.csv("Data-screening-1 (1).csv")
mastercopy <- master

#Checking names of columns and remove all those column which are not in appendix or not relative to study.
names(mastercopy)

#dropping unecesscory rows
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
library("stringr")
mastercopy$Age <-str_replace_all(mastercopy$Age, "[abcdefgfhijklmanopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]", NA_character_)
mastercopy$Age <-strtoi(mastercopy$Age)
#It makes no sense if the person age more than 100 or less tha 5 contribute in survey.
mastercopy$Age[(mastercopy["Age"] < 5)] = NA
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

#Summary of the data
summary(mastercopy1)
  
#Applying Mice to fill null values
install.packages("mice")
library(mice)
nomissdf= mice(mastercopy1)
df1=complete(nomissdf,1)
summary(df1)
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

# G Communication related plastics 
names(mastercopy2[61:66])
res.pca7 <- prcomp(mastercopy2[61:66],scale= TRUE)  
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

#DA
#try1
library(MASS)
da1 <- lda(Q14C~Gender+Age+Education+Occupation+Income,data=mastercopy2)
da1

#LDA preduction
lda.testing <- predict(da1)
#confusion matrix
accuracy <- table(lda.testing$class,mastercopy2$Q14C)
accuracy
sum(accuracy[row(accuracy) == col(accuracy)]) / sum(accuracy)


