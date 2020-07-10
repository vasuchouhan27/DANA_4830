# Checking Accuracy of Data

#Import the data and make copy of data
master <- read.csv("D:/Term3/Stats 1/New folder/Data-screening-1 (1).csv")
mastercopy <- master

#Checking names of columns and remove all those column which are not in appendix or not relative to study.
names(mastercopy)

#droppping unnesscry rows
mastercopy <- mastercopy[,-c(1:16), drop=FALSE]
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
mastercopy<-mastercopy[!(mastercopy["Age"] <5),]
mastercopy<-mastercopy[!(mastercopy["Age"] >100),]
#Income
#as one enntry is "30000" it falls under the category of 3 so to change it.
mastercopy$Income[mastercopy$Income=="30000"] <- "3"
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
write.csv(mastercopy1,"C:\\Users\\vasu2\\Desktop\\mastercopy1.csv", row.names = FALSE)
mastercopy1<-read.csv("C:\\Users\\vasu2\\Desktop\\mastercopy1.csv",stringsAsFactors = FALSE)
library("naniar")
vis_miss(mastercopy1)

# To delete the columns which are having more tha 90% DAta 
# To delete the rows which are having more than 50% data
mastercopy1<-mastercopy1[which(rowMeans(!is.na(mastercopy1)) > 0.5), ]
mastercopy1<-mastercopy1[,which(colMeans(!is.na(mastercopy1)) > 0.1)]
vis_miss(mastercopy1)
dim(mastercopy1)

#to fill zero in multiple choice questions
#We Start with Q5
names(mastercopy1[10:16])
mastercopy1[10:16][is.na(mastercopy1[10:16])] <- 0
lapply(mastercopy1[10:16],summary)

#with Q12
lapply(mastercopy1,names)[33:38]
mastercopy1[33:38][is.na(mastercopy1[33:38])] <- 0
lapply(mastercopy1[33:38],summary)

#with Q13
names(mastercopy1[39:44])
mastercopy1[39:44][is.na(mastercopy1[39:44])] <- 0

#with Q15
names(mastercopy1)[46:51]
mastercopy1[46:51][is.na(mastercopy1[46:51])] <- 0
lapply(mastercopy1[46:51],summary)

#with Q16
names(mastercopy1)[52:56]
mastercopy1[52:56][is.na(mastercopy1[52:56])] <- 0
lapply(mastercopy1[52:56], summary)

#with Q19
names(mastercopy1)[66:71]
mastercopy1[66:71][is.na(mastercopy1[66:71])] <- 0
dim(mastercopy1)
vis_miss(mastercopy1)

#Make Another copy of data
mastercopy2<-mastercopy1
# To delete the columns which are having more tha 90% DAta 
# To delete the rows which are having more than 50% data
mastercopy2<-mastercopy2[which(rowMeans(!is.na(mastercopy2)) > 0.5), ]
mastercopy2<-mastercopy2[,which(colMeans(!is.na(mastercopy2)) > 0.1)]
vis_miss(mastercopy2)
dim(mastercopy2)

#As we can see, there are no row and column falls under this category.

#Fill out all missing values with "MICE"
library(mice)
impute <- mice(mastercopy2,m=3)
print(impute)
final <- complete(impute,1)
vis_miss(final)
dim(final)

#Finally import the Final data set
write.csv(final,"D:\\Term3\\DANA_4830_Project\\final.csv", row.names = FALSE)

m <- read.csv("D:\\Term3\\DANA_4830_Project\\final.csv")


dim(m)


# this is by manoj 