# 설문
a <- read.csv('clipboard',sep='\t')
str(a)
write.csv(a,'csv/ques.csv',row.names = F)

a <- read.csv('csv/ques.csv')
items <- c('남','여')
a$문1 <- as.factor(a$문1)
levels(a$문1) <- items
b <- table(a$문1)
b
b / sum(b) * 100

items <- c('흰색','검은색','붉은색','청색')
a$문2 <- as.factor(a$문2)
levels(a$문2) <- items
b <- table(a$문2)
b
b / sum(b) * 100

items <- c('20대','30대','40대','50대')
a$문3 <- as.factor(a$문3)
levels(a$문3) <- items
b <- table(a$문3)
b
b / sum(b) * 100

items <- c('TV광고','지인추천','개인의지','기타')
a$문4 <- as.factor(a$문4)
levels(a$문4) <- items
b <- table(a$문4)
b
b / sum(b) * 100

items <- c('매우만족','만족','보통','불만','매우불만')
a$문5 <- as.factor(a$문5)
levels(a$문5) <- items
b <- table(a$문5)
b
b / sum(b) * 100

# 평균
(95+90+85)/3
a <- c(95,90,85)
sum(a)/length(a)
mean(a)

# 분산
((95-90)^2+(90-90)^2+(85-90)^2)/(3-1)
m <- mean(a)
sum((a-m)^2)/(3-1)
var(a)
sd(a)

# 상관관계
x<-c(1,2,2,4,6)
y<-c(9,9,2,1,4)
sx <- sd(x)
sy <- sd(y)
sxy <- cov(x,y)
sxy/sx/sy
cor(x,y)

# 회귀분석
a <- read.csv('clipboard',sep='\t')
a[,4] <- as.numeric(gsub(',','',a[,4]))
str(a)
colnames(a)
write.csv(a,'csv/salary.csv',row.names = F)

a <- read.csv('csv/salary.csv')
library("lm.beta")
res = lm.beta(lm("월.지출.만.원.~월급.만.원.",a,))
summary(res)

x = seq(-3,3,0.1)
plot(x,dt(x,df=9))
qt(0.975,df = 9)
pt(2.262,df=9)

# beta
b <- a
b[,2] <- (b[,2]-mean(b[,2])) / sd(b[,2])
b[,3] <- (b[,3]-mean(b[,3])) / sd(b[,3])
resb = lm.beta(lm("월.지출.만.원.~월급.만.원.",b))
summary(resb)


# 다중 회귀분석
a = read.csv('clipboard',sep='\t')
a = a[-1,]
a[,2] <- as.numeric(gsub(',','',a[,2]))
a[,3] <- as.numeric(gsub(',','',a[,3]))
a[,4] <- as.numeric(gsub(',','',a[,4]))
str(a)
colnames(a)
summary(a)
write.csv(a,'csv/population.csv',row.names=F)

a = read.csv('csv/population.csv')
res = lm.beta(lm("취업자~X15세.이상.인구+경제활동인구",a,))
summary(res)

x = seq(-3,3,0.1)
plot(x,dt(x,df=7))
qt(0.975,df = 7)
pt(2.365,df=7)

