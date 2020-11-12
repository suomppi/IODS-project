#Alex Suomalainen 
#12.11.2020
#Logistic regression
#source: Paulo Cortez, University of Minho, GuimarÃ£es, Portugal, http://www3.dsi.uminho.pt/pcortez

#Data Wrangling part
#reading the files
f <- file.choose('student-mat.csv')
sm <- read.csv(f, sep = ";", header = T)
f2 <- file.choose('student-por.csv')
sp <- read.csv(f2, sep = ";", header = T)

#exporing structure and dimensions
str(sm)
str(sp)
#the student-mat data has 395 observations of 33 variables
#the student-por data has 649 observations of 33 variables

#joining the dataset. first assessing libraries
library(dplyr)
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
math_por <- inner_join(sm, sp, by = c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet") , copy = FALSE, suffix = c(".math", ".por"))

#new column names
colnames(math_por)

#new dataset of only the joined columns
alc <- selec

#columns that were not used for joining the data
notjoined_columns <- colnames(sm)[!colnames(sm) %in% join_by]

#printing out the columns not used for joining
notjoined_columns

#for every column name not used for joining
for( column_name in notjoined_columns) {
  #select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  #select the first column vector of those 2 columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

#checking the new data

glimpse(alc)

#assessing tidyverse packages
library(ggplot2); library(dplyr)

#creating a new column alc_use by averaging weekly and monthly alcohol consumptio
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2 )

#ggplot of alcohol use
g1 <- ggplot(alc, aes(x=alc_use)) 
g1 + geom_bar()

#defining a new column = high_use
alc <- mutate(alc, high_use = alc_use > 2)

#plot of high_use

g2 <- ggplot(alc, aes(x = high_use)) 
g2 + geom_bar()

#plot of high use with sex
g2 + geom_bar() + facet_wrap("sex")

#checking that everthing is in order
str(alc)
#the data has 382 obs. of 35 variables as it should

#saving the data
write.csv(alc, 'alc.csv')


