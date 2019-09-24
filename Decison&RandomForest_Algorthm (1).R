
library(ISLR)
head(College)

df<-College

# ----------Exploratory
library(ggplot2)
ggplot(df, aes(Room.Board, Grad.Rate))+geom_point(aes(color=Private), size =4,alpha=0.5)

ggplot(df, aes(F.Undergrad))+geom_histogram(aes(fill=Private), color='Black', bins = 50, alpha=0.5)+theme_bw()

ggplot(df, aes(Grad.Rate))+geom_histogram(aes(fill=Private), color='Black', bins = 50, alpha=0.5)+theme_bw()

subset(df, Grad.Rate>100)

df['Cazenovia College', 'Grad.Rate'] <- 100
subset(df, Grad.Rate>100)

#------------------Split tool
 library(caTools)
set.seed(101)

sample <- sample.split(df$Private, SplitRatio = .70)
train<- subset(df, sample == T)
test<-subset(df, sample == F)

#----------Decision tree -- Install rpart package 
library(rpart)

tree <- rpart(Private ~., method = "class", data = train)
tree.pred <- predict(tree, test)
head(tree.pred)

###############
tree.pred <- as.data.frame(tree.pred)

###### joiner - join of 2 columns

joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else {
    return('No')
  }
}

######### 
tree.pred$Private <- sapply(tree.pred$Yes, joiner)
print(head(tree.pred))

################# confusion matrix 
table(tree.pred$Private, test$Private)

#-------------Creating tree --------Install rpart.plot 
library(rpart.plot)
prp(tree) # -------- if doesnot found 

plot(tree, uniform = T, main ='Tree')
text(tree, use.n = T, all = T)


#------------------Creating Random Forest---------install randomforest
library(randomForest)

rf.model <- randomForest(Private~., data=train, importance=TRUE)
rf.model$confusion
rf.model$importance

rf.preds <- predict(rf.model, test)
table(rf.preds, test$Private)