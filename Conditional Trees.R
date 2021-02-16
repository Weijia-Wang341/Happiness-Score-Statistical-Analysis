happy <- read.csv(file = '~/Downloads/2019.csv', header = TRUE)

library(party)
fit.ctree <- ctree(Score.index~  GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity, data=training) 
plot(fit.ctree, main="Conditional Inference Tree")

ctree.pred <- predict(fit.ctree, validation, type="response")
ctree.perf <- table(validation$Score.index, ctree.pred,dnn=c("Actual", "Predicted"))
ctree.perf
fraction1 <- mean(ctree.pred == validation$Score.index)
fraction1

