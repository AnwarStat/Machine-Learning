#install.packages('tidyverse')
#install.packages('tidytext')
#install.packages("syuzhet")

## Importing packages
library(tidyverse) # metapackage with lots of helpful functions
library(tidytext) # tidy implimentation of NLP methods
library(syuzhet) #syuzhet: Extracts Sentiment and Sentiment-Derived Plot Arcs from Text.

# read in our data
news <- read_csv("D:/Anwar/Statistics/Research/Fake News Protection/fake.csv")

head(news)
cols(
  .default = col_character(),
  ord_in_thread = col_double(),
  published = col_datetime(format = ""),
  crawled = col_datetime(format = ""),
  domain_rank = col_double(),
  spam_score = col_double(),
  replies_count = col_double(),
  participants_count = col_double(),
  likes = col_double(),
  comments = col_double(),
  shares = col_double()
)
news<-news[sample(1:nrow(news)), ]
news$type<-factor(news$type)
news<-news[1000,]
head(news)
str(news)
#bs and conspiracy news are also fake
news$type<-gsub("bs","fake",news$type)                 
news$type<-gsub("conspiracy","fake",news$type)          
#while others are real
news$type<-gsub("bias","real",news$type)              
news$type<-gsub("satire","real",news$type)
news$type<-gsub("hate","real",news$type)
news$type<-gsub("junksci","real",news$type)
news$type<-gsub("state","real",news$type)


#Count of type of news that how many are fake and real
news %>% group_by(type) %>% summarise(count=n())


#apply function for finding question marks and exclamations and adding into our dataframe
news$exc <- sapply(news$text, function(x) length(unlist(strsplit(as.character(x), "\\!+")))) #count exclamation
news$que <- sapply(news$text, function(x) length(unlist(strsplit(as.character(x), "\\?+")))) #count question marks



##Count of exclamations in fake and real news
news %>% group_by(type) %>% summarise(exclamations=sum(exc))


#Count of question marks in fake and real news
news %>% group_by(type) %>% summarise(QuestionMarks=sum(que))

#boxplot for exclamations in fake and real news
boxplot(exc ~ type,news,ylim=c(0,20),ylab="",col=c("red","orange"))
#we can observe that fake news have more exclamations than real news

#boxplot for question marks in fake and real news
boxplot(que ~ type,news,ylim=c(0,20),col=c("red","orange")) 
#we can observe that fake news have more question marks than real

#function for finding words in each text
terms<- function(fake, text_column, group_column){
  
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- news %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>%
    ungroup()
  
  # get the number of words per text
   total_words <- words %>%
   group_by(!!group_column) %>%
   summarize(total = sum(n))
  
  # combine the two dataframes we just made
  return (words)
}



#store all words per text in different data frame
memory.limit(size=20000)
df<-terms(news,text,type)
head(df)
str(df)
#create boxplot for number of words of each type
boxplot(n ~ type,df,log="y",xlab="type",ylab="number of words",col=c("green","pink"))

#create sentiment table for text column
sentiment<-get_nrc_sentiment(news$text)
sentiment

#taking only last two columns negative and positive for the analysis
df1<-sentiment[c(9,10)]
head(df1)
#function for normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#normalize negative and positive column for better analysis means the values will lie between 0 and 1
df1$negative<-normalize(df1$negative)
df1$positive<-normalize(df1$positive)


#Combine this with the news dataset
news<-cbind(news,df1)
head(news)
#finding standard deviations and median of negative and positive columns for each type of news 
neg_sd<-news %>% group_by(type) %>% summarise(neg_sd=sd(negative))
pos_sd<-news %>% group_by(type) %>% summarise(pos_sd=sd(positive))
neg_med<-news %>% group_by(type) %>% summarise(neg_med=median(negative))
pos_med<-news %>% group_by(type) %>% summarise(pos_med=median(positive))

#create dataframes for negative and positive standard deviations and median
dfr2<-data.frame(neg_sd)
dfr1<-data.frame(pos_sd)
dfr3<-data.frame(neg_med)
dfr4<-data.frame(pos_med)
#merging dataframes and taking transpose of t1 we get t2
t1<-merge(dfr1,dfr2)
t2<-t(t1)
t2

#merging dataframes and taking transpose of t4 we get t3
t3<-merge(dfr4,dfr3)
t4<-t(t3)
t4


library(ggplot2)
library(lattice)
library(caret)
#dataset
sum(is.na(news$shares))

dataset<-data.frame(type=news$type,news$ord_in_thread,
spam_srore=news$spam_score,
replies=news$replies_count,
participants=news$participants_count,
like=news$likes,
comments=news$comments,
shares=news$shares,
exc=news$exc,
que=news$que)
head(dataset)

dataset1<-cbind(dataset,sentiment)
head(dataset1)
type<-news[,20]
dataset2<-cbind(type,sentiment)
head(dataset2)
colnames(dataset2)
levels(dataset2$type)<-c('bias','bs','conpiracy','fake','hate','ts','satire','state')
#create a validation data set

# create a list of 80% of the rows in the original dataset we can use for training
dataset = dataset2[sample(1:nrow(dataset2)), ]
validation_index <- createDataPartition(dataset$type, p=0.80, list=FALSE)
validation_index
# select 20% of the data for validation
validation <- dataset[-validation_index,]
validation
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]
dataset
# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)

# take a peek at the first 5 rows of the data
head(dataset)

# list the levels for the class
levels(dataset$type)

# summarize the class distribution
percentage <- prop.table(table(dataset$type)) * 100
cbind(freq=table(dataset$type), percentage=percentage)

# summarize attribute distributions
summary(dataset)

#Univariate Plots
# split input and output
x <- dataset[,-1]
y <- dataset[,1]

# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(dataset)[i+1])
}

# barplot for class breakdown
par(mfrow=c(1,1))
plot(y)

# scatterplot matrix
par(mfrow=c(1,1))
featurePlot(x=x, y=y,plot = if (is.factor(y)) "ellipse" else "scatter",
            labels = c("Feature", ""))
featurePlot(x,y,"strip")
featurePlot(x,y,"density")
featurePlot(x,y,"pairs")
# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#Build Models

#Linear Discriminant Analysis (LDA)
#Classification and Regression Trees (CART).
#k-Nearest Neighbors (kNN).
#Support Vector Machines (SVM) with a linear kernel.
#Random Forest (RF)

# a) linear algorithms
set.seed(7)
fit.lda <- train(type~., data=dataset, method="lda", metric=metric, trControl=control)
fit.lda
#summary(fit.lda)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(type~., data=dataset, method="rpart", metric=metric, trControl=control)
fit.cart
#summary(fit.cart)

# kNN
set.seed(7)
fit.knn <- train(type~., data=dataset, method="knn", metric=metric, trControl=control)
fit.knn
#summary(fit.knn)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(type~., data=dataset, method="svmRadial", metric=metric, trControl=control)
fit.svm

#summary(fit.svm)
# Random Forest
set.seed(7)
fit.rf <- train(type~., data=dataset, method="rf", metric=metric, trControl=control)
fit.rf
#summary(fit.rf)

## summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.lda)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
predictions
confusionMatrix(predictions, validation$type)

