#Alex Suomalainen
#5.11.2020
#R-studio exercise 2

#read the data into R
mvdata <- read.table("https://www.mv.helsinki.fi/home/kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
#exploring data
mvdata
str(mvdata)
dim(mvdata)
#this data has 183 rows and 60 columns

#Create an analysis dataset with the variables 
#gender, age, attitude, deep, stra, surf and points 
#by combining questions in the learning2014 data


#creating column "attitude", by scaling the column "attitude"
mvdata$attitude <- mvdata$Attitude / 10


#creating column "deep" by combining questions
library(dplyr)
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
#taking the mean and creating the column "deep"
deep_columns <- select(mvdata, one_of(deep_questions))
mvdata$deep <- rowMeans(deep_columns)

#creating column "stra" by scaling the column "strategy"
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
#averaging and creating the column "stra"
strategic_columns <- select(mvdata, one_of(strategic_questions))
mvdata$stra <- rowMeans(strategic_columns)

#creating column "surf" by scaling and averaging the column "surface"
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
surface_columns <- select(mvdata, one_of(surface_questions))
mvdata$surf <- rowMeans(surface_columns)
mvdata$surf

#Selecting to keep these columns
keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")
#creating new dataset of these columns
mvnew <-select(mvdata, one_of(keep_columns))
#filtering out zeros
mvnew <- filter(mvnew, Points > 0)

#changing column names
colnames(mvnew)
colnames(mvnew)[2] <- "age"
colnames(mvnew)[7] <- "points"
colnames(mvnew)

#checking the structure
library(tidyverse)
view(mvnew)
str(mvnew)

#I searched youtube for a quide to set a working directory
#I found a 2 minute video, which told me everything I needed

#saving the dataset to the 'data' folder
write.csv(mvnew,'learning2014.csv')

#checking that everythings correct
read.table('learning2014.csv')
str('learning2014.csv')
head('learning2014.csv')

#reading the data into r
library()
learning2014 <- read.csv('learning2014.csv', TRUE, ",")

#cheking structure and stuff
str(learning2014)
head(learning2014)
dim(learning2014)

#the data has eight variables and 166 observations 
#(one variable is 'x' I have no clue where it came from, and will delete it)
keep_columns <- c("gender","age","attitude", "deep", "stra", "surf", "points")
learning2014 <- select(learning2014, one_of(keep_columns))
learning2014

#cheking the dimensions again..
dim(learning2014)
#now the data has 7 variables: "gender","age","attitude"
#"deep", "stra", "surf", "points", and 166 observations

#graphical overview of the data
library(tidyverse)
summary(learning2014)
#this gave me a summary of the whole data
#for example the median age is 22.00 
#mean strategic learning score is 3.121  
#max. points is 33.00
 
#creating a regression model with multiple explanatory variables
mun_malli <- lm(learning2014$points ~ learning2014$age + learning2014$deep + learning2014$stra, data = learning2014)
summary(mun_malli)

#drawing a diasnostic plot
#drawing a QQ plot
plot(mun_malli)




