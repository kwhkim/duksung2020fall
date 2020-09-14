head(mtcars)

write.csv(mtcars, file='file_cars.csv')

mtcars2 = mtcars
mtcars2[1,1]=0
mtcars2[1,2]=0
mtcars2[3,1]=0
mtcars2
View(mtcars2)
write.csv(mtcars2, file='file_cars2.csv')
write.csv(mtcars2, file='file_cars2.csv')

write.table(mtcars2, file='file_cars3.csv', sep=' ')
write.table(mtcars2, file='file_cars4.csv', sep='\t')

read.csv(file='file_cars2.csv', sep=' ')

dat = read.csv(file='file_cars2.csv')
dat = read.csv(file='file_cars2.csv', row.names=1)
dat
head(dat)
#all.equal(mtcars2, dat)

install.packages('dplyr')
library(dplyr)

mtcars[1,1]

dat <- mtcars %>% select(mpg, cyl, disp)
dat = mtcars %>% select(mpg, cyl, disp)
mtcars %>% select(mpg:disp)
 mtcars %>% select(-mpg, -cyl, -disp)

dat2 <- mtcars %>% slice(1,2,3,5:10)
dat2 <- mtcars %>% slice(-(1:10))

dat3 <- mtcars %>% filter(vs == 0)

mtcars %>% mutate(v = hp + disp)

mtcars %>% select(hp, disp) %>% mutate(v=hp+disp)

x <- 3

dat = mtcars %>% 
  select(am, hp) %>%
  mutate(am2 = ifelse(am==0, "auto", "manual"))

head(dat)

dat %>% 
  group_by(am2) %>%
  summarise(mean(hp))

summarise(group_by(dat, am2), mean(hp))

#g(f(sd(mean(dat))), 2)
#dat %>% mean %>% sd %>% f %>% g(2)

# comment
#mean, sd, var, median

mtcars %>% summarise(mean(hp))
mtcars %>% summarise(sd(hp))
mtcars %>% summarise(median(hp))
mtcars %>% summarise(var(hp))
mtcars %>% group_by(am) %>% summarise(var(hp))


mtcars %>% 
  group_by(am) %>% 
  summarise(mean(hp), sd(hp), mean(disp))

# mtcars에서 cyl별로 hp의 평균을 구하세요.

data(mtcars)
mtcars

mtcars %>% group_by(am) %>% 
  summarise(mean(hp))

mtcars %>% group_by(am) %>% 
  summarise(mean(hp), sd(hp))

