bf <- read.csv(file.choose(),header=T)
attach(bf)
lm(Y~.,bf)
model1 <- lm(Y~.,bf)
summary(model1)
anova(model1)

rstudent(model1)


influence.measures(model1)

PRESS.statistic <- sum( (resid(bf)/(1-hatvalues(bf)))^2 )
print(paste("PRESS statistic= ", PRESS.statistic))

plot(model1)
par(mfrow=c(2,2))


(bf.backward1 <- step(model1, scope = list(lower ~ X1), trace=0))

plot(fitted(model1) ~ fitted(bf.backward1))
abline(0,1)

cor(fitted(model1), fitted(bf.backward1))

print(vif(model1))

lm.ridge(Y~.,bf,lambda = 0, model = FALSE)
ridge <- lm.ridge(Y~.,bf,lambda = seq(0,0.001,0.0001))
summary(ridge)
plot(ridge)

boxcox(Y~X1+X2+X6, data=bf, lambda = seq(-2, 2, 1/10), plotit = TRUE, eps = 1/50, xlab = expression(lambda),ylab = "log-Likelihood")
attach(bf1)
