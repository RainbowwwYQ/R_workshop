

# data type: character/string
a <- "hello"
print(a)
a

b <- c("hello","world")
b
b[1]

# integer/numbers
a <- 3
a
b <- sqrt(a*a+3)
b

a <- c(1,2,3,4,5)
a
a+1
mean(a)
var(a)

# logical

a <- TRUE
typeof(a)
b = FALSE
typeof(b)

# have a try!
rm(list = ls())

a = c(1,2,3)
class(a)
is.numeric(a)
is.character(a)

a <- as.character(a)
class(a)
# vectors

c(1,2,3)
c("Data", 24, TRUE)

# list

y <- list(name="Mike", gender="M", company="ProgramCreek", age=24)

# matrix
#my_matrix <- matrix(1:6, nrow=3, ncol=2)

# dataframe
# example1

name <- c("Mike", "Lucy", "John") 
age <- c(20, 25, 30) 
student <- c(TRUE, FALSE, TRUE) 
df = data.frame(name, age, student) 

df = data.frame(name=c("Mike", "Lucy", "John"), 
                age = c(20, 25, 30), 
                student = c(TRUE, FALSE, TRUE)) 
df

# example2
df2 <- data.frame(name=c("ash","jane","paul","mark"), score=c(67,56,87,91))

# example 3
a <- c(1,2,3,4)
b <- c(2,4,6,8)
levels <- factor(c("A","B","A","B"))
bubba <- data.frame(first=a,
                    second=b,
                    f=levels)
bubba
summary(bubba)
bubba$first
bubba$second
bubba$f

# basic computation

# Basic Numerical Descriptions
tree <- read.csv("trees91.csv", header=TRUE, sep=",")
names(tree)

tree$LFBM
mean(tree$LFBM)
median(tree$LFBM)
quantile(tree$LFBM)
min(tree$LFBM)
max(tree$LFBM)
var(tree$LFBM)
sd(tree$LFBM)

summary(tree$LFBM)
summary(tree)

round(12.009)
round(12.009, digit = 2)
round(12.599)
round(12.599, 1)

floor(12.001)
floor(12.599)
floor(12.999)

ceiling(12.001)
ceiling(12.599)
ceiling(12.999)

# control structures

# if-else loop
temperature <- 0
if (temperature > 30){
  print("It is sooooo hot! Stay home!")
} else {
  print("It is not so hot! Enjoy your pleasant time!")
}

# for loop
score <- c(98,95,20,25,30)
for (i in 1:5){
  print (score[i])
}

# practice: if-loop + for-loop
obs <- length(score)
for (i in 1:obs){
  if (score[i]>60){
    print("Congraduation! You've passed the exam!")
  } else {
    print("Sorry, you fail to pass this exam...")
  }
}

# while loop

readinteger <- function(){
  n <- readline(prompt="Please, enter your ANSWER: ")
}
response <- as.integer(readinteger())

while (response!=42) {   
  print("Sorry, the answer to whatever the question MUST be 42");
  response <- as.integer(readinteger());
}


# let's start a practice!!!

rm(list = ls())
df <- read.csv("data processing.csv", header = TRUE, sep = ",")

#df2 <- read.csv(file = "/Users/mayhe/Dropbox/R basic workshop/data processing.csv", header = T, as.is = T, encoding = "UTF=8")


# warm-up exercises
attributes(df)
# dim(df)  # nrows, ncolumns
class(df)
nrow(df)
ncol(df)
colnames(df)
row.names(df)
row.names(df)[2] <- "999"

is.na(df$Age)
table(is.na(df$Age))
na.omit(df)
df2 <- df[complete.cases(df$Age),]

summary(df)

unique(df$Country)
length(df$Country)
length(unique(df$Country))

# identify the sequence of your table
sort(df$Age)  # numbers from smallest to largest
order(df$Age)  # rows from smallest to largest
df3 <- df[order(df$Age),]
row.names(df3) <-NULL
rank(df$Age)  # ranks for each row
# df4 <- df[rank(df$Age),] # not a good choice!

# reorder by column name
df <- df[c("Age","Purchased","Salary","Country")]
df <- df[c(1,4,2,3)]


# basic steps for data pre processing
# remove missing values
options(digits=2)  # default value is 7

df$Age[is.na(df$Age)] = mean(df$Age, na.rm = T)
df$Salary[is.na(df$Salary)] = mean(df$Salary, na.rm = T)

# encoding categorical values
df$Country = factor(df$Country,
                    levels = c('France', 'Spain', 'Germany'),
                    labels = c(1, 2, 3))
df$Purchased = factor(df$Purchased,
                      levels = c('No', 'Yes'),
                      labels = c(0, 1))

#Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(df$Purchased, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Feature Scaling /standardization
training_set[, 3:4] = scale(training_set[, 3:4])
test_set[, 3:4] = scale(test_set[, 3:4])

# introduction for R useful packages

# stringr for strings

library(stringr)
df <- read.csv("data processing.csv", header = TRUE, sep = ",")

str_detect(df$Country, "an")  # Detect the presence of a pattern match in a string
str_detect(df$Country[[2]], "an")

#example
df$if_an <- NA
for (i in 1:10){
  if (str_detect(df[i,"Country"], "an")!=TRUE){
    df[i,"if_an"] <- "N"
  } else {
    df[i,"if_an"] <- "Y"
  }
}

df$id <- paste(df$Country, df$Age, sep = "-")
toupper(df$Country)
df$Country <- toupper(df$Country)
tolower(df$Country)

df$sub <- str_sub(df$Country, 1, -3) # Extract substrings from a character vector

df$len <- str_length(df$Country) # The width of strings 

df$replace <- str_replace(df$Country, "AN", "oo") # Replace the first matched pattern in each string.

strsplit(df$id, "-")
str_split_fixed(df$id, "-", 2)
str_split_fixed(df$id, "-", 2)[,1]

# you can also use separate () and separate_rows() to split the rows

# tidyr for data tidying

library(tidyr)

# reshape data
df0 <- spread(df, Country, Salary)
df0 <- gather(df0, 'France','Germany','Spain',key="Country", value="Salary")
df0 <- df0[complete.cases(df0),]

# next...handle missing values
df <- read.csv("data processing.csv", header = TRUE, sep = ",")

df0 <- drop_na(df) # remove directly

df0 <- fill(df, Age)
df0 <- fill(df0, Salary)  # more recent non-NA values

df0 <- replace_na(df, list(Age=999))

# dplyr for data manipulation

library(dplyr)

# manipulate cases
distinct(df$Country)
df$Country <- as.character(df$Country)
class(df$Country)
distinct(df, Country)   # remove rows with duplicate values

top_n(df, 5, Age) # select andn order top n entries
arrange(df, Age) # order rows by values of a column or columns
arrange(df, desc(Age))

# combine tables

bind_cols(df, df0)# paste tables beside each other as they are
bind_rows(df, df0)

df$id <- c(1:10)
df0$id <- c(1:10)

left_join(df, df0, by="id")

# ggplot2 for visualization

library(ggplot2)
housing <- read.csv("landdata-states.csv")

# scatter plots
# ggplot2

ggplot(housing,
       aes(x=Date,
           y=Home.Value,
           color=State))+
  geom_point()

# if we just want two states...
ggplot(filter(housing, State %in% c("MA", "TX")),
       aes(x=Date,
           y=Home.Value,
           color=State))+
  geom_point()


hp2001Q1 <- filter(housing, Date == 2001.25) 
ggplot(hp2001Q1,
       aes(y = Structure.Cost, x = Land.Value)) +
  geom_point()

p1 <- ggplot(hp2001Q1, aes(x = log(Land.Value), y = Structure.Cost))
p1 +
  geom_point(aes(color=Home.Value, shape = region))

# histogram

p2 <- ggplot(housing, aes(x = Home.Value))
p2 + geom_histogram(stat = "bin", binwidth=4000)

# do you want to see some amazing graphs???
# are you ready?

p5 <- ggplot(housing, aes(x = Date, y = Home.Value))
p5 <- p5 + geom_line() +
  facet_wrap(~State, ncol = 10)
p5

p5 + theme_linedraw()

dev.off()
# go to https://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html to find more graphs!
# or google ggplot2 tutorials, you will find more funs!

# built-in datesets
data() 
data(mtcars)
head(mtcars)
# ?mtcars
