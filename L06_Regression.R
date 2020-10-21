## LEC 06.

## 스트레스와 건강 간 관계
dat <- read.table("https://www.uvm.edu/~dhowell/fundamentals8/DataFiles/Tab10-2.dat",
                  sep="\t", header=T)

## 학교에 대한 재정 지원
dat <- read.table("https://www.uvm.edu/~statdhtx/fundamentals8/DataFiles/Tab11-1.dat",
                  sep="\t", header=T)
head(dat)
plot(Symptoms ~ Stress, data=dat)
skimr::skim(dat)
library(GGally)
ggpairs(dat)

library(dplyr)
ggpairs(dat %>% select(-id))

# 1. 회귀분석의 목적은?
#    a. 인과 관계 분석
#    b. 예측 모형 개발
#    c. 인과 + 예측 (ex. ANCOVA)

# 2. OLS 선형회귀분석의 가정?
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




