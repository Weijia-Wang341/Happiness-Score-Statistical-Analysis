happy <- read.csv(file = '~/Downloads/2019.csv', header = TRUE)

library(randomForest)
set.seed(1234)
fit.forest <- randomForest(Score.index~GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity,
                           data=training,na.action=na.roughfix, importance=TRUE)
fit.forest
importance(fit.forest, type = 2)

forest.pred <- predict(fit.forest, validation)
forest.perf <- table(validation$Score.index, forest.pred, dnn=c("Actual", "Predicted"))
forest.perf
fraction1 <- mean(forest.pred == validation$Score.index)
fraction1

forest.pred <- predict(fit.forest, validation)
forest.perf <- table(validation$Score.index, forest.pred, dnn=c("Actual", "Predicted"))
forest.perf
fraction1 <- mean(forest.pred == validation$Score.index)
fraction1
```

Using performance() function to find out which model is better. 
```{r}
performance <- function(table, n = 2){
  if (!all(dim(table)==c(2,2)))
    stop('Must be a 2 x 2 table')
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n) ,
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}
performance(dtree.perf)
performance(ctree.perf)
performance(forest.perf)

