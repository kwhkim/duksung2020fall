## 혼합효과분석 R 스크립트
# 신정아. (2019). 혼합효과모형 (Mixed-Effects Model) 을 이용한 실험언어학 데이터 분석 방법 고찰: 자기조절읽기 실험 데이터를 중심으로. 영어학, 19(1), 76–94.

library(lme4); library(lmerTest)

data<-read.csv(file=file.choose(), sep = ',',  header = T, fill=T) #data는 http://bitly.kr/LMEdata
data$Subject <- factor(data$Subject)
data$Item <- factor(data$Item)

#data$Proficiency <- factor(data$Proficiency, levels=levels(data$Proficiency)[c(2,1)])
#
unique(data$Proficiency)
data$Proficiency <- factor(data$Proficiency)
levels(data$Proficiency)
library(forcats)
data$Proficiency <- fct_relevel(data$Proficiency, "lo", "hi")
levels(data$Proficiency)
head(data)

attach(data)
mean(age)
range(age)





########Accuracy 분석
ACCmean <- aggregate(ACC~Type+Proficiency, data=data, mean); ACCmean
ACCsd <- aggregate(ACC~Type+Proficiency, data=data, sd); ACCsd
ACCtable <- tapply(ACC, list(Type, Proficiency), mean); ACCtable
ACCbar <- barplot(ACCtable, ylab="Accuracy", ylim=c(0, 1), col=rainbow(2), legend=rownames(ACCtable), args.legend = list(x ='topright', bty='n', inset=c(-.3,-.4)), beside=T)

require(dplyr)
require(tidyr)
ACCmean2 <- data %>% group_by(Type, Proficiency) %>% summarise(ACCmean = mean(ACC))
ACCsd2 <- data %>% group_by(Type, Proficiency) %>% summarise(ACC = sd(ACC))
ACCtable2 <- ACCmean2 %>% spread(key='Proficiency', value='ACCmean')


#effect coding 
data$Typec <- ifelse((data$Type == 'unambiguous'), -.5, .5) #effect/deviation coding
data$Profc <- ifelse((data$Proficiency == 'lo'), -.5, .5)
#options(contrasts=c("contr.sum", "contr.poly")) # set sum contrasts 도 할 수 있음

#Model selection (모형 안에 함수는 수렴 문제를 일으키지 않기 위해 넣는 기능으로 사용하였다.)

m.full=glmer(ACC~Typec+(1+Typec|Subject)+(1+Typec*Profc|Item),
             data,
             family="binomial",
             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.full) #singular fit이므로 over-fitted 되서 선택하지 않음

m.subj=glmer(ACC~Typec+(1+Typec|Subject)+(1+Profc|Item),
             data,
             family="binomial",
             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))
summary(m.subj) #singular fit이므로 over-fitted 되서 선택하지 않음

m.item.full=glmer(ACC~Typec*Profc+(1|Subject)+(1+Typec*Profc|Item),
                  data,
                  family="binomial",
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) 
summary(m.item.full) #singular fit이므로 over-fitted 되서 선택하지 않음

m.item.1=glmer(ACC~Typec*Profc+(1|Subject)+(1+Typec+Profc|Item),
               data,
               family="binomial",
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) 
m.item.2=glmer(ACC~Typec*Profc+(1|Subject)+(1+Profc|Item),
               data,
               family="binomial",
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) 

anova(m.item.1, m.item.2) #모델 비교후 1 선택

m.item.3=glmer(ACC~Typec*Profc+(1|Subject)+(1+Typec|Item),
               data,
               family="binomial",
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) 

anova(m.item.1, m.item.3) #1, 3 모델 비교후 3번 선택
anova(m.item.2, m.item.3) #2, 3 모델 비교후 3번 선택

m.item.0=glmer(ACC~Typec*Profc+(1|Subject)+(1|Item),data,family="binomial",control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) #단순 모형

anova(m.item.0, m.item.3) #2, 3 모델 비교후 3번 선택

ACCfinal.model <- m.item.3 #최종 모델 3번 선택
summary(ACCfinal.model) 

######## 모형 가정 검증

#1. 잔차 분산의 동질성 (logistic 모형은 확인하기 쉽지 않음)
plot(ACCfinal.model)

#2. 잔차 정규분포
x<-resid(ACCfinal.model); qqnorm(x); qqline(x, col=2)

#3. 잔차의 공선성(collinearity)의 불출현: 예측변수들의 상호관련성이 없어야한다
library(car)
sqrt(vif(ACCfinal.model))>2 #2이상이면 다중공선성 문제

# 다른 방법
library(ggResidpanel) # install.packages('ggResidpanel')
resid_panel(ACCfinal.model)
resid_xpanel(ACCfinal.model)



########RT 분석
RTdata <- subset(data, ACC == 1)
# RTdata <- data %>% filter(ACC == 1)
(nrow(data)-nrow(RTdata))/nrow(data) #오답으로 버려진 데이터 비율

class(RTdata$RT)
RTdata$RT<-as.numeric(as.character(RTdata$RT)) 

RTda= subset(RTdata, 200<=RT & RT<=2000) # 절대적인 Outlier 제거
#RTda <- RTdata %>% filter(200<=RT & RT <=2000)
#RTda= subset(RTdata, 100<=RT & RT<=2000) # 절대적인 Outlier 제거

(nrow(RTdata)-nrow(RTda))/nrow(RTdata) #outlier으로 버려진 데이터 비율

# relative outlier 제거하기  
splitlist = split(RTda$RT, list(RTda$Subject),drop = TRUE)
cuthigh = lapply(splitlist, function(x){mean(x)+2.5*sd(x)}) #gives high cutoffs per subject
cutlow = lapply(splitlist, function(x){mean(x)-2.5*sd(x)}) #gives high cutoffs per subject
RTda$cuthigh = unsplit(cuthigh,list(RTda$Subject))
RTda$cutlow = unsplit(cutlow,list(RTda$Subject))
RT<-RTda[(RTda$RT<=RTda$cuthigh & RTda$RT>=RTda$cutlow), ]

data_cut = RTda %>% 
  group_by(Subject) %>% 
  summarise(cuthigh = mean(RT)+2.5*sd(RT),
            cutlow = mean(RT)-2.5*sd(RT))
RT2 <- left_join(RTda, data_cut, by='Subject') %>% filter(RT <= cuthigh & RT >= cutlow)


(nrow(RTdata)-nrow(RT))/nrow(RTdata) #outlier으로 버려진 데이터 비율

RTmean <- aggregate(RT~Type+Proficiency, data=RT, mean); RTmean

RTsd <- aggregate(RT~Type+Proficiency, data=RT, sd); RTsd

####centering function

myCenter = function(x) x - mean(x, na.rm=T)
emplog = function(p, totalN) {
  y = p*totalN
  return(log( (y+.5)/ (totalN-y + .5) ))
}
emplogweight = function(p, totalN) {
  y = p*totalN
  return(1 / ( 1 / (y+.5) + 1 / (totalN-y + .5) ))
}

RT$cType =  myCenter(ifelse(RT$Type == "unambiguous", -.5, .5))
RT$cProf =  myCenter(ifelse(RT$Proficiency == "lo", -.5, .5))

#정규성 확인 및 비교
library(lattice)
qqmath(~RT, RT) 
qqmath(~RRT, RT)
qqmath(~logRRT, RT)
densityplot(~RT, RT)
densityplot(~RRT, RT)
densityplot(~logRRT, RT)

#모형 선택
logRRTmodel.full<-lmer(logRRT~cType*cProf + (1+cType|Subject) +(1+cType*cProf|Item), data=RT, control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) #singular fit 이므로 선택에서 제외

logRRTmodel.1<-lmer(logRRT~cType*cProf + (1+cType|Subject) +(1+cType+cProf|Item), data=RT, control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6)))  #singular fit 이므로 선택에서 제외

logRRTmodel.item<-lmer(logRRT~cType*cProf + (1|Subject) +(1+cType|Item), data=RT, control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) #singular fit 이므로 선택에서 제외

logRRTmodel.item.1<-lmer(logRRT~cType*cProf + (1|Subject) +(1+cProf|Item), data=RT, control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) #singular fit 이므로 선택에서 제외

logRRTmodel.simple<-lmer(logRRT~cType*cProf + (1|Subject) +(1|Item), data=RT, control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) 

summary(logRRTmodel.simple) #모델 선택


## ICC
mod.simple1 <- lmer(logRRT~(1|Subject) , data=RT, control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) 
mod.simple2 <- lmer(logRRT~(1|Item), data=RT, control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) 
mod.simple3 <- lmer(logRRT~(1|Subject) +(1|Item), data=RT, control=lmerControl(optimizer="bobyqa",optCtrl=list(maxfun=1e6))) 
require(performance)
icc(mod.simple1)
summary(mod.simple1)
# (0.0027460+0.0002351) / (0.0027460+0.0002351 + 0.0753446)

icc(mod.simple2)
icc(mod.simple3)

library(psychometric) # install.packages('psychometric')
ICC1.lme(logRRT, Subject, data)
ICC2.lme(logRRT, Subject, data)

ICC1.lme(logRRT, Item, data)
ICC2.lme(logRRT, Item, data)




####Model assumption 점검
library(car)

#1. 선형성 linearity 및 2. 잔차 분산의 동질성(homogeneity), 등분산성(homoscedasticity)

attach(RT)
model <- logRRTmodel.simple
plot(model)

#3. 잔차 정규성
x<-resid(model)
qqnorm(x)
qqline(x, col=2)

#4. 잔차의 공선성(collinearity)의 불출현: 예측변수들의 상호관련성이 없어야한다

sqrt(vif(model))>2 #2이상이면 다중공선성 문제이나, 문제 없음.


