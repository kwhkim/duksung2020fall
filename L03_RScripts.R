library(dplyr)
# seq = sequence(from, to, by)
source('funcs.R') # functions
#getmode
getmode
a(3)
b(4)
#
rm(x) # remove

(mtcars  
  %>% select(am, hp, cyl))

dat = 
  mtcars %>% 
  select(am, hp, cyl) %>% 
  slice(1:100) %>% 
  filter(am==1) %>% 
  mutate(hp2 = hp^2) %>%
  group_by(cyl) %>% 
  summarise(a=mean(hp), mean(hp2), 
            median(hp), iqr(hp)) 

dat %>% select(`mean(hp)`)
dat %>% select(a)

dat %>% rename(iqr = `iqr(hp)`)


dat = 
  mtcars %>% 
    select(am, hp, cyl) %>% 
    slice(1:100) %>% 
    filter(am==1) %>% 
    mutate(hp2 = hp^2) %>%
    group_by(cyl) %>% 
    summarise(mhp= mean(hp), mhp2=mean(hp2), 
              med=median(hp), iqr = iqr(hp)) 




# mean, median, getmode
x = c(3,5,7,5,4,3,5,2,6,7,4,2) # concatenate
mean(x)
median(x)
#mode(x)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(x)

# [CTRL] + [S]
# [CTRL] + [SHIFT] + [F10] 

# var(variance), sd(standard deviation)
# max()-min() (range)
x <- mtcars$mpg
var(x)
sd(x) # std(x)
max(x)-min(x)
range(x)
range2 = function(x) {
  max(x) - min(x)
}
range2(x)
iqr = IQR
iqr(x)
IQR(x) #Inter-Quantile Range
quantile(x, probs=0.75) - quantile(x, probs=0.25)

summary(x)
# install.packages('psych')
library(psych)
describe(x)
describe(mtcars)
summary(mtcars)
# install.packages('skimr')
library(skimr)
skimr::skim(mtcars)
psych::describe(mtcars)
summary(mtcars)

mtcars2 <- mtcars %>% 
  mutate(am2 = factor(ifelse(am==0, 'automatic', 'manual')))

mtcars2 <- mtcars %>% 
  mutate(am2 = factor(am))


summary(mtcars2)
mtcars2$am2


# install.packages('e1071')
# 설치가 잘 되지 않는다면,
# 1. Rstudio를 다시 시작한다. [CTRL]+[SHIFT]+[F10]
# 2. Rstudio를 관리자 권한으로 시작한다.
library(e1071) 

skewness(x) # 왜도
kurtosis(x) # 첨도

data(mtcars)
hist(mtcars$hp, breaks= 30)
hist(mtcars$hp, breaks= 5)
hist(mtcars$hp, breaks= c(50,100,200,400))
hist(mtcars$wt)

#install.packages('ggplot2')
#gg : grammar of graphics
library(ggplot2)
ggplot(data=mtcars, aes(x=hp)) + 
  geom_histogram(bins=10)
ggplot(data=mtcars, aes(x=wt)) + 
  geom_histogram(bins=3)
ggplot(data=mtcars, aes(x=wt)) + 
  geom_histogram(binwidth=1)
ggplot(data=mtcars, aes(x=wt)) + 
  geom_histogram(binwidth=0.5)


mtcars2 <- mtcars %>% 
  mutate(am2 = factor(ifelse(am==0, 'automatic', 'manual')),
         cyl2 = factor(cyl))

#mutate = 3
#rm(mutate)

#ggplot = function(x) {x+1}
#ggplot(mtcars)
#rm(ggplot)

# aes = aesthetic
ggplot(data = mtcars2) + 
  geom_point(aes(x=hp, y=wt, col=am2, shape=cyl2)) + 
  geom_line(aes(x=hp, y=qsec), col='blue') + 
  labs(title='mtcars')

ggplot(data = mtcars2) + 
  geom_boxplot(aes(x=am2, y=wt), width=0.2) + 
  labs(title='mtcars')

ggplot(data = mtcars2) + 
  geom_boxplot(aes(x=am2, y=wt, col=cyl2), width=0.6) + 
  labs(title='mtcars') 

ggplot(data = mtcars2) + 
  geom_point(aes(x=hp, y=wt), alpha=0.6) + 
  #facet_wrap(~ cyl2) + 
  facet_grid(am2  ~cyl2) + 
# facet_grid
  labs(title='mtcars') 



ggplot(data = mtcars2) + 
  geom_boxplot(aes(x=cyl2, y=wt), width=0.2) + 
  labs(title='mtcars')

ggplot(data = mtcars2) + 
  geom_point(aes(x=cyl, y=wt), width=0.2) + 
  labs(title='mtcars')


colnames(mtcars) # column names

# 

skewness(mtcars$hp) # positive skew
kurtosis(mtcars$hp)


help(kurtosis)
?kurtosis


t.test(mtcars$wt)
t.test(wt ~ 1, mtcars)

t.test(wt ~ am2, mtcars2)

# wt ~ am2
plot(wt ~ am2, mtcars2)
# ggplot2로 boxplot, 산점도

#facet_grid(y~x)

# 다음 주 발표 : D조 






