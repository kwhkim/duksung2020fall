## LEC 06.
# https://github.com/kwhkim/duksung2020fall

## 슬라이드의 키와 체중
dat <-
  data.frame(height = c(157, 166, 174, 191, 185, 196, 179, 153, 164, 180),
             weight = c(74, 53, 70, 79, 75, 79, 63, 40, 66, 71))
plot(weight ~ height, dat)
# formula = 공식
# data = 데이터
lm(formula = weight ~ height, data = dat)
#dat <- dat %>% mutate(x2 = x1 + alla)
# y ~ x
# y ~ x1 + x2 + x3
# * y ~ I(x1 + x2)

summary(lm(formula = weight ~ height, data = dat))
fit <- lm(formula = weight ~ height, data = dat)
summary(fit)
abline(fit)

#OLS(Ordinary Least Squares)

## 스트레스와 건강 간 관계
dat <- read.table("https://www.uvm.edu/~dhowell/fundamentals8/DataFiles/Tab10-2.dat",
                  sep="\t", header=T)
head(dat)
plot(Symptoms ~ Stress, data=dat)
fit1 <- lm(Symptoms ~ Stress, data=dat)
summary(lm(Symptoms ~ Stress, data=dat))

x <- rnorm(100)
y <- 3*x^2 + rnorm(100)
dat = data.frame(x=x, y=y)
plot(y~x, dat)

fit2 <- lm(y ~ x, data=dat)
plot(fit)


## 학교에 대한 재정 지원
dat <- read.table("https://www.uvm.edu/~statdhtx/fundamentals8/DataFiles/Tab11-1.dat",
                  sep="\t", header=T)




skimr::skim(dat)
#install.packages('GGally')
library(GGally)
ggpairs(dat)

library(dplyr)
ggpairs(dat %>% select(-id))

# 1. 회귀분석의 목적은?
#    a. 인과 관계 분석
#    b. 예측 모형 개발
#    c. 인과 + 예측 (ex. ANCOVA)

# 2. OLS 선형회귀분석의 가정?
#    LINE.
#    a. Linearity
#    b. Independence
#    c. Normality
#    d. Equal Variance

# 3. 독립변인 : 연속형, 범주형
#    종속변인 : 연속형

# 4. R 활용 분석
##   4.a. 기본
fit <- lm(Symptoms ~ Stress, data = dat)
summary(fit)
plot(fit)

##   4.b. 기타
library(lmtest)
library(car)
coef(fit)
coefci(fit, level=0.95)
coeftest(fit)
vcov(fit)
# vif(fit)
plot(fit, which=1:6)
influenceIndexPlot(fit)
outlierTest(fit)

## 5. model fit
plot(fit)


#plot(allEffects(fit))
#points(Symptoms ~ Stress, data=dat)




