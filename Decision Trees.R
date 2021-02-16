happy <- read.csv(file = '~/Downloads/2019.csv', header = TRUE)

library(ISLR)
set.seed(1234)
train <- sample(nrow(happy), 0.8*nrow(happy)) 
training <- happy[train,]
validation <- happy[-train,] 
table(training$Score.index)
table(validation$Score.index)

library('rpart')
library('rpart.plot')
set.seed(1234)
dtree <- rpart(Score.index ~ GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity, 
               data=training, method="class",parms=list(split="information"))
dtree$cptable
print(dtree)
plotcp(dtree)
dtree.pruned <- prune(dtree, cp=.0125) 
prp(dtree.pruned, type = 2, extra = 104,fallen.leaves = TRUE, main="Decision Tree")

dtree.pred <- predict(dtree.pruned, validation, type="class")
dtree.perf <- table(validation$Score.index, dtree.pred,dnn=c("Actual", "Predicted"))
dtree.perf