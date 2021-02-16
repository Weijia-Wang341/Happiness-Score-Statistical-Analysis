happy <- read.csv(file = '~/Downloads/2019.csv', header = TRUE)

happy$Score.index <- factor(happy$Score> mean(happy$Score), levels=c(TRUE,FALSE),labels=c("High", "Low"))
head(happy)

fit.full<- glm(Score.index ~ GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity , data=happy, family=binomial())
summary(fit.full)

fit.null <- glm(Score.index ~ 1, data=happy, family=binomial()) 
anova(fit.null, fit.full, test="Chisq")

fit.reduced <- glm(Score.index ~ Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices,
                   data = happy, family=binomial())
summary(fit.reduced)

anova(fit.reduced, fit.full, test="Chisq")

exp(coef(fit.reduced))
