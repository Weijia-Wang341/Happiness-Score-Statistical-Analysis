happy <- read.csv(file = '~/Downloads/2019.csv', header = TRUE)

happy$GDP.rank[happy$GDP.per.capita] <- 1
happy$GDP.rank[happy$GDP.per.capita > 0.493] <- 2
happy$GDP.rank[happy$GDP.per.capita > 0.831] <- 3
happy$GDP.rank[happy$GDP.per.capita > 1.050] <- 4
happy$GDP.rank[happy$GDP.per.capita > 1.269] <- 5
happy$SS.rank[happy$Social.support] <- 1
happy$SS.rank[happy$Social.support > 0.939] <- 2
happy$SS.rank[happy$Social.support > 1.203] <- 3
happy$SS.rank[happy$Social.support > 1.346] <- 4
happy$SS.rank[happy$Social.support > 1.465] <- 5
happy$expectancy.rank[happy$Healthy.life.expectancy] <- 1
happy$expectancy.rank[happy$Healthy.life.expectancy > 0.486] <- 2
happy$expectancy.rank[happy$Healthy.life.expectancy > 0.723] <- 3
happy$expectancy.rank[happy$Healthy.life.expectancy > 0.825] <- 4
happy$expectancy.rank[happy$Healthy.life.expectancy > 0.92] <- 5
happy$freedom.rank[happy$Freedom.to.make.life.choices >=0] <- 1
happy$freedom.rank[happy$Freedom.to.make.life.choices > 0.269] <- 2
happy$freedom.rank[happy$Freedom.to.make.life.choices > 0.370] <- 3
happy$freedom.rank[happy$Freedom.to.make.life.choices > 0.450] <- 4
happy$freedom.rank[happy$Freedom.to.make.life.choices > 0.516] <- 5
happy$generosity.rank[happy$Generosity >= 0 ] <- 1
happy$generosity.rank[happy$Generosity > 0.099] <- 2
happy$generosity.rank[happy$Generosity > 0.153] <- 3
happy$generosity.rank[happy$Generosity > 0.198] <- 4
happy$generosity.rank[happy$Generosity > 0.261] <- 5

with(happy,{
  df <- cbind(aggregate(Score, by=list(GDP.rank), FUN=mean),
              aggregate(Score, by=list(GDP.rank), FUN=sd))
  df <- df[,c(1,2,4)]
  colnames(df) = c("GDP.rank","mean", "sd")
  df 
})

library('gplots')
with(happy,{
  plotmeans(Score ~ GDP.rank, xlab="GDP Ranking", ylab="Score",main="Mean Plot\nwith 95% CI")
})

fit.aov <- aov(happy$Score ~ happy$GDP.rank) 
summary(fit.aov)

with(happy,{
  df <- cbind(aggregate(Score, by=list(SS.rank), FUN=mean),
              aggregate(Score, by=list(SS.rank), FUN=sd))
  df <- df[,c(1,2,4)]
  colnames(df) = c("SS.rank","mean", "sd")
  df 
})
library('gplots')
with(happy,{
  plotmeans(Score ~ SS.rank, xlab="Social Support Ranking", ylab="Score",main="Mean Plot\nwith 95% CI")
})

fit.SS.aov <- aov(happy$Score ~ happy$SS.rank) 
summary(fit.SS.aov)

with(happy,{
  df <- cbind(aggregate(Score, by=list(expectancy.rank), FUN=mean),
              aggregate(Score, by=list(expectancy.rank), FUN=sd))
  df <- df[,c(1,2,4)]
  colnames(df) = c("expectancy.rank","mean", "sd")
  df 
})
library('gplots')
with(happy,{
  plotmeans(Score ~ expectancy.rank, xlab="Life Expectancy Ranking", ylab="Score",main="Mean Plot\nwith 95% CI")
})

fit.expect.aov <- aov(happy$Score ~ happy$expectancy.rank) 
summary(fit.expect.aov)

with(happy,{
  df <- cbind(aggregate(Score, by=list(freedom.rank), FUN=mean),
              aggregate(Score, by=list(freedom.rank), FUN=sd))
  df <- df[,c(1,2,4)]
  colnames(df) = c("freedom.rank","mean", "sd")
  df 
})
library('gplots')
with(happy,{
  plotmeans(Score ~ freedom.rank, xlab="Freedom Ranking", ylab="Score",main="Mean Plot\nwith 95% CI")
})

fit.freedom.aov <- aov(happy$Score ~ happy$freedom.rank) 
summary(fit.freedom.aov)

with(happy,{
  df <- cbind(aggregate(Score, by=list(generosity.rank), FUN=mean),
              aggregate(Score, by=list(generosity.rank), FUN=sd))
  df <- df[,c(1,2,4)]
  colnames(df) = c("generosity.rank","mean", "sd")
  df 
})
library('gplots')
with(happy,{
  plotmeans(Score ~ generosity.rank, xlab="Generosity Ranking", ylab="Score",main="Mean Plot\nwith 95% CI")
})

fit.generosity.aov <- aov(happy$Score ~ happy$generosity.rank) 
summary(fit.generosity.aov)