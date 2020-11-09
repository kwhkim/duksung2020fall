library(ggplot2)
library(dplyr)
library(tidyr)
dat <- read.table("https://www.uvm.edu/~statdhtx/fundamentals9/DataFiles/Tab18-1.dat",
                  sep="\t", header=T)

n = nrow(dat)
means = data.frame(lapply(dat, mean)) %>% gather(key='key', value='value', Week0:Week12) %>%
  mutate(key=factor(key, levels = c('Week0', 'Week3', 'Week6', 'Week9', 'Week12'))) %>%
  rename(mean = value)
ses = data.frame(lapply(dat, sd)) %>% gather(key='key', value='value', Week0:Week12) %>%
  mutate(key=factor(key, levels = c('Week0', 'Week3', 'Week6', 'Week9', 'Week12'))) %>%
  rename(se = value) %>%
  mutate(se = se/sqrt(n))

datPlot = inner_join(means, ses, by='key')

ggplot(datPlot) + 
  geom_point(mapping = aes(x=key, y=mean)) + 
  geom_errorbar(mapping = aes(x=key, ymin = mean-se, ymax = mean+se), width=0.2) 
  

dat$subj = 1:nrow(dat)

dat2 <- dat %>% gather(key='key', value='value', Week0:Week12) 
str(dat2)

dat2 %>% mutate(key = factor(key, level=c('Week0', 'Week3', 'Week6', 'Week9', 'Week12'))) %>%
  ggplot(aes(x=key, y=value)) + 
  geom_boxplot(width=0.2) +
  geom_jitter(width=0.2, alpha=0.5) +
  labs(title = '지진 전후의 우울 점수(놀란-훽시마 등, 1991)',
       subtitle = '주별 상자 그림과 데이터')


ggplot(datPlot) + 
  geom_point(mapping = aes(x=key, y=mean), size=3) + 
  geom_errorbar(mapping = aes(x=key, ymin = mean-se, ymax = mean+se), 
                size=1.4, width=0.2)  +
  geom_jitter(data = dat2 %>% 
                mutate(key = factor(key, level=c('Week0', 'Week3', 'Week6', 'Week9', 'Week12'))),
              width=0.2, alpha=0.5, mapping = aes(x=key, y=value)) +
  labs(title = '지진 전후의 우울 점수(놀란-훽시마 등, 1991)',
       subtitle = '주별 표본 평균과 표준 오차, 그리고 데이터')

  

dat2 %>% mutate(key = factor(key, level=c('Week0', 'Week3', 'Week6', 'Week9', 'Week12'))) %>%
  ggplot(aes(x=key, y=value)) + 
  geom_boxplot(width=0.2, alpha=0.5) +
  geom_line(aes(col=factor(subj), group=factor(subj))) + 
  geom_point(alpha=0.5, aes(col=factor(subj))) +
  labs(title = '지진 전후의 우울 점수(놀란-훽시마 등, 1991)',
       subtitle = '주별 상자 그림과 개인별 데이터')


library(gghighlight)
dat2$emptylabel = ""

dat2 %>% mutate(key = factor(key, level=c('Week0', 'Week3', 'Week6', 'Week9', 'Week12'))) %>%
  ggplot(aes(x=key, y=value)) + 
  geom_point(alpha=0.5,
             mapping = aes(col = factor(subj))) +
  facet_wrap(~subj) + 
  gghighlight(label_key=emptylabel) +
  labs(title = '지진 전후의 우울 점수(놀란-훽시마 등, 1991)',
       subtitle = '개인별 우울 점수: 주별 변화') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none') 


library(stringr)
dat2 %>% 
  mutate(key = str_replace(key,'Week', '')) %>%
  mutate(key = as.numeric(key)) %>% 
  ggplot(aes(x=key, y=value, col=factor(subj),
             group=factor(subj)))+ 
  geom_point(alpha=0.5) +
  geom_line() + 
  facet_wrap(~subj) + 
  gghighlight(label_key=emptylabel) + 
  scale_x_continuous(breaks=c(0,3,6,9,12)) +
  labs(title = '지진 전후의 우울 점수(놀란-훽시마 등, 1991)',
       subtitle = '개인별 우울 점수: 주별 변화',
       x='Week',
       y='depression') + 
  theme(legend.position = 'none')


