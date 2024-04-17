stiffness <- c(11, 12, 15, 14, 17, 20, 18, 14, 18, 11, 17, 14, 16, 13, 15, 19)
tresult <- t.test(stiffness,mu=12)
tresult

t95 <- qt(0.975,df=15)
t95

(1-pt(4.695,df=15))*2

x <- seq(-5,5,0.1)
y <- dt(x,df=15)
plot(x,y,type='l')
x <- seq(-5,-t95,0.01)
y <- dt(x,df=15)
polygon(c(x,-t95,-5),c(y,0,0),col='red',border='black')
x <- seq(t95,5,0.01)
y <- dt(x,df=15)
polygon(c(x,5,t95),c(y,0,0),col='red',border='black')
abline(v=tresult$statistic,col='blue')

korea <- c(22, 19, 16, 17, 19, 16, 26, 24, 18, 19, 13, 16, 22, 18, 19, 22, 19, 26)
seoul <- c(22, 20, 28, 24, 22, 28, 22, 19, 25, 21, 23, 24, 23, 23, 29, 23)
df1 <- data.frame(stiffness=korea)
df1[['company']] <- 'korea'
df2 <- data.frame(stiffness=seoul)
df2[['company']] <- 'seoul'
df <- rbind(df1,df2)
df[['company']] = as.factor(df[['company']])
df

library('car')

lv <- leveneTest(stiffness~company,center=mean,data=df)
lv

t.test(korea,seoul,var.equal=F)

tresult2 <- t.test(korea,seoul,var.equal=T)
tresult2

qt(0.975,df=32)
pt(-3.6008,df=32)*2

ex1 <- c(76,57,72,47,52,76,64,64,66,57,38,58)
ex2 <- c(89,60,71,65,60,70,71,69,68,66,50,62)
result4 <- t.test(ex1,ex2,paired=TRUE)
result4

qt(0.975,df=11)

pt(-3.2832,df=11)*2

c <- c(3.6,4.1,4.0,3.1,3.2,3.9,3.2,3.5,3.5,3.5,3.8,3.8)
df <- data.frame(cholesterol=c)
df[['company']] <- rep(LETTERS[1:4],each=3)
df

av <- aov(df[['cholesterol']]~df[['company']])
summary(av)

qf(0.95,df1=3,df2=8)

1-pf(2.25,df1=3,df2=8)

str(table(df))

tbl <- rbind(c(457,446),c(43,54))
tbl <- as.table(tbl)
dimnames(tbl)<-list(c('모른다','알고있다'),c('남자','여자'))
names(dimnames(tbl)) <- c('응답','성별')
tbl

chisq.test(tbl)

qchisq(0.95,df=1)

1-pchisq(1.1417,df=1)

tbl <- rbind(c(28,36,11),c(22,44,9))
tbl <- as.table(tbl)
dimnames(tbl)<-list(c('참석','불참'),c('1학년','2학년','3학년'))
names(dimnames(tbl)) <- c('응답','학년')
tbl

chisq.test(tbl)

qchisq(0.95,df=2)

1-pchisq(1.72,df=2)

tbl <- rbind(c(20,6,7,9),c(6,33,14,7),c(9,7,29,8),c(8,8,10,24))
tbl <- as.table(tbl)
dimnames(tbl)<-list(c('스포츠','독서','음악','영화'),
                    c('1학년','2학년','3학년','4학년'))
names(dimnames(tbl)) <- c('취미','학년')
tbl

chisq.test(tbl)

qchisq(0.95,df=9)

1-pchisq(79.454,df=9)


