# 1
load("C:/Users/jihun/Downloads/winequality.RData")
quality <- wine$quality 
facidity <- wine$fixed.acidity
vacidity <- wine$volatile.acidity
citric <- wine$citric.acid
rsugar <- wine$residual.sugar
chlorides <- wine$chlorides
fso2 <- wine$free.sulfur.dioxide
tso2 <- wine$total.sulfur.dioxide
density <- wine$density
pH <- wine$pH
so4 <- wine$sulphates
alcohol <- wine$alcohol
# linear model
lmod <- lm(quality ~ facidity + vacidity + citric + rsugar + chlorides + fso2 + tso2 + density + pH + so4 + alcohol)
summary(lmod)
# model selection
step(lmod)
lmod4 <- lm(quality ~ vacidity + chlorides + fso2 + tso2 + pH + so4 + alcohol)
summary(lmod4)
lmod=lmod4
# diagnostics
# homoscedasticity
par(mfrow=c(1,2))
plot(fitted(lmod),residuals(lmod),xlab="Fitted",ylab="Residuals",main="Residual Plot")
qqnorm(residuals(lmod))
qqline(residuals(lmod))
# normality
shapiro.test(residuals(lmod))
# independence
library(lmtest)
dwtest(quality ~ vacidity + chlorides + fso2 + tso2 + pH + so4 + alcohol)

# outliers
# check leverages
halfnorm(lm.influence(lmod)$hat,ylab="Leverages")
# outlier
jack <- rstudent(lmod)
jack[which.max(abs(jack))]
tail(jack[order(abs(jack))])
qt(.05/(1599*2),1599-8)
jack[152]
# influence
cook <- cooks.distance(lmod)
halfnorm(cook, ylab="Cook's distances")
summary(lm(quality ~ vacidity + chlorides + fso2 + tso2 + pH + so4 + alcohol, subset=(cook<max(cook))))



# bootstrap
library(boot)
# function to obtain regression weights 
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=wine, statistic=bs, 
                R=1000, formula=quality ~ vacidity + chlorides + fso2 + tso2 + pH + so4 + alcohol)

# view results
results
plot(results, index=6) # pH



lmod = lm(quality ~ vacidity + chlorides + fso2 + tso2 + pH + so4 + alcohol)
preds = fitted(lmod)
resids = residuals(lmod)
vec=numeric(10000)
for(i in 1:10000) {
  ynew = preds + sample(resids, rep=TRUE)
  vec[i]=summary(lm(ynew ~ vacidity + chlorides + fso2 + tso2 + pH + so4 + alcohol))$coef[6]
}
par(mfrow=c(1,1))
hist(vec)
a = -0.48266
length(vec[vec>a])/10000
sort(vec)[250] 
sort(vec)[9750]
# Box-cox transformation
library(MASS)
boxcox(lmod, plotit=T)
head(cbind(boxcox(lmod)$x, boxcox(lmod)$y)[order(-boxcox(lmod)$y),])
boxcox(lmod,plotit=T,lambda=seq(0.5,1.5,by=0.1))

# data-driven approach

quality_ = ((quality -1)^0.91)/0.91
fit_ = lm(quality_ ~ vacidity + chlorides + fso2 + tso2 + pH + so4 + alcohol)
summary(fit_)
par(mfrow=c(1,2))
plot(fit_$fitted.values,fit_$residuals,main="Transformed Fitted Value vs Residuals")
qqnorm(fit_$residuals)
qqline(residuals(fit_))

library(Metrics)

trainwine <- wine[1:1400,]
testwine <- wine[1401:1599,]

mod1 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine)
mod2 <- lm(((quality -1)^0.91)/0.91 ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine)

rmse(predict(mod1,testwine), testwine$quality) # 0.6524137
rmse(predict(mod2,testwine), testwine$quality) # 1.283869

trainwine2 <- wine[-c(1201:1400),]
testwine2 <- wine[c(1201:1400),]
mod21 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine2)
mod22 <- lm(((quality -1)^0.91)/0.91 ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine2)

rmse(predict(mod21,testwine2), testwine2$quality) # 0.631
rmse(predict(mod22,testwine2), testwine2$quality) # 1.269

trainwine3 <- wine[-c(1001:1200),]
testwine3 <- wine[c(1001:1200),]
mod31 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine3)
mod32 <- lm(((quality -1)^0.91)/0.91 ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine3)

rmse(predict(mod31,testwine3), testwine3$quality) # 0.623
rmse(predict(mod32,testwine3), testwine3$quality) # 1.495

trainwine4 <- wine[-c(801:1000),]
testwine4 <- wine[c(801:1000),]
mod41 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine4)
mod42 <- lm(((quality -1)^0.91)/0.91 ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine4)

rmse(predict(mod41,testwine4), testwine4$quality) # 0.692
rmse(predict(mod42,testwine4), testwine4$quality) # 1.412

trainwine5 <- wine[-c(601:800),]
testwine5 <- wine[c(601:800),]
mod51 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine5)
mod52 <- lm(((quality -1)^0.91)/0.91 ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine5)

rmse(predict(mod51,testwine5), testwine5$quality) # 0.613
rmse(predict(mod52,testwine5), testwine5$quality) #1.296

trainwine6 <- wine[-c(401:600),]
testwine6 <- wine[c(401:600),]
mod61 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine6)
mod62 <- lm(((quality -1)^0.91)/0.91 ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine6)

rmse(predict(mod61,testwine6), testwine6$quality) # 0.679
rmse(predict(mod62,testwine6), testwine6$quality) # 1.425

trainwine7 <- wine[-c(201:400),]
testwine7 <- wine[c(201:400),]
mod71 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine7)
mod72 <- lm(((quality -1)^0.91)/0.91 ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine7)

rmse(predict(mod71,testwine7), testwine7$quality) # 0.666
rmse(predict(mod72,testwine7), testwine7$quality) # 1.53

trainwine8 <- wine[-c(1:200),]
testwine8 <- wine[c(1:200),]
mod81 <- lm(quality ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine8)
mod82 <- lm(((quality -1)^0.91)/0.91 ~ volatile.acidity + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data=trainwine8)

rmse(predict(mod81,testwine8), testwine8$quality) # 0.662
rmse(predict(mod82,testwine8), testwine8$quality) # 1.207

library(analogue)

pcrmod <- pcr(quality ~ volatile.acidity + chlorides + 
                free.sulfur.dioxide + total.sulfur.dioxide +
                pH + sulphates + alcohol, data=trainwine,
              validation="CV",ncomp=50)

rmse(predict(pcrmod, testwine$quality))



# 2
library(faraway)
data(penicillin)
penicillin
plot(yield ~ treat, penicillin,pch=unclass(blend))
plot(yield ~ blend, penicillin,pch=unclass(treat))
# RCBD
lmod <- lm(yield ~ treat + blend, penicillin)
summary(lmod)
lmod_ <- lm(yield ~ blend, penicillin)
anova(lmod,lmod_)

# CRD
lmod2 <- lm(yield ~ treat, penicillin)
summary(lmod2)
anova(lmod2)

# random effect


library(nlme)
gls(yield ~ treat + blend, penicillin)

glmod <- gls(yield ~ treat, penicillin)
summary(glmod) #5.533 = 30.61

lmod <- lm(yield ~ treat + blend, penicillin)
summary(lmod) #4.34 = 18.83


# 3
load("C:/Users/jihun/Downloads/Constrained.RData")
y_ = y - x2
x_ = x1 - x2
summary(lm(y_ ~ x_))

