# Alex Suomalainen
# 26.11.2020
# exercise 5

# reading the data
human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt"
                    , sep  =",", header = T)

#exploring structure and dimensions
str(human)
dim(human)
# the data has 195 observations and 19 variables
# there are characher, integrer and numerical variables

# assessing packages
library(stringr)
library(dplyr)

# transforming the GNI variable into numeric
str_replace(human$GNI, pattern=",", replace ="") %>%
  as.numeric(human$GNI)

# excluding unneeded variables
# columns to keep
keep <- c("Country", "Edu2.FM", "Labo.FM", "Life.Exp", "Edu.Exp",
          "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")

# selecting these columns
human <- select(human, one_of(keep))

# printing out the data along with a completeness indicator as the last column
data.frame(human[-1], comp = complete.cases(human))

#filtering out the rows with NA values
human_ <- filter(human, TRUE)

tail(human, n = 10)
#the last 7 observations are regions, not countries so lets exclude them
human_ <- human[1:155, ]

#adding countries as rownames and overwright
rownames(human_) <- human_$Country
human <- human_
#removing 'country' variable
human_ <- select(human, -Country)

#checking that everthing is correct
dim(human_)
human_
