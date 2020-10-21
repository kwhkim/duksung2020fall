# Lec05_Regression

## Correlations

#dat
dat <- read.table("https://www.uvm.edu/~dhowell/fundamentals8/DataFiles/Tab11-7.dat",
                  sep="\t", header=T)
head(dat)
str(dat) # structure
# integer : 정수
# numeric : 수치형
# chracter : 문자형
dat$Efficacy = as.character(dat$Efficacy)
dat$Efficacy = as.numeric(dat$Efficacy)

library(dplyr)

cor(dat %>% select(-FAMID))
cor(dat %>% select(Esteem:Efficacy))
cov(dat) #var(dat)

cor(dat$Esteem, dat$MatCare) # -1 ~ 1
cov(dat$Esteem, dat$MatCare)
sd(dat$Esteem)
sd(dat$MatCare)
cov(dat$Esteem, dat$MatCare)/(sd(dat$Esteem)*sd(dat$MatCare))

x = rnorm(100)
y = rnorm(100)
plot(y ~ x)

x = rnorm(100)
y = x+ rnorm(100)
plot(y ~ x)

x = rnorm(100)
y = 2*x+ rnorm(100)
plot(y ~ x)

psych::corr.test(dat %>% select(-FAMID) %>% slice(1:20))
psych::cor.ci(dat)
psych::cor.ci(dat %>% select(Esteem, MatCare))

library(psych)
corr.test(dat$Esteem, dat$MatCare)

library(corrgram)
# if the output message is something like,
#   "Error in library(corrgram) : there is no package called 'corrgram'
#   install.packages('corrgram')
corrgram(dat, order=T, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
car::scatterplotMatrix(dat %>% select(-FAMID))

mean(dat$MatCare)
dat <- 
  dat %>% mutate(
  MatCare = ifelse(MatCare > 3.318,
                   'High', 'Low'))

pairs(dat %>% select(-FAMID)) #plot(dat)
lattice::splom(dat %>% select(-FAMID))

library(ggplot2)
library(GGally)
ggpairs(dat)
dat$cat = factor(sample(c("A", "B", "C"), size=nrow(dat), replace=TRUE))

library(plotly)
d <- highlight_key(dat)
p <- ggpairs(d, columns = 1:5)
p <- ggpairs(d)
ggplotly(p) %>% 
  highlight("plotly_selected")

d <- highlight_key(dat)
p <- ggpairs(d, aes(colour = cat), columns = 1:4)
ggplotly(p) %>% 
  highlight("plotly_selected")


## Simple Regression

dat2 <- data.frame(
  height = c(157, 166, 174, 191, 185, 196, 179, 153, 164, 180),
  weight = c(74, 53, 70, 79, 75, 79, 63, 40, 66, 71)
)

fit2 <- lm(weight ~ height, data=dat2)
summary(fit2)
coef(fit2)

dat3 <- dat2[sample(nrow(dat2), nrow(dat2), replace=TRUE),]
fit3 <- lm(weight ~ height, data=dat2)
summary(fit3)

est = c()
for (i in 1:1000) {
  dat3 <- dat2[sample(nrow(dat2), nrow(dat2), replace=TRUE),]
  fit3 <- lm(weight ~ height, data=dat3)
  est[i] = coef(fit3)['height']
}
hist(est)


## Multiple Regression
lmFit0 <- lm(Efficacy ~ MatCare , data=dat)
lmFit1 <- lm(Efficacy ~ MatCare + Esteem , data=dat)

summary(lmFit0)
summary(lmFit1)

library(lmtest) #install.packages("lmtest")
library(car)

vif(lmFit1)
coefci(lmFit1)
vcov(lmFit1)

plot(lmFit1)
plot(lmFit1, which=1:6)

compareCoefs(lmFit0, lmFit1)

plot(lmFit1)
plot(lmFit1, which=1:6)

qqPlot(lmFit1)
influenceIndexPlot(lmFit1, id.n=3)
car::residualPlots(lmFit1)
anova(lmFit0, lmFit1)
car::outlierTest(lmFit1)

