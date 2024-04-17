df = read.csv('csv/timeseries.csv')

str(df)

# 순번
df[['X']] <- 1:nrow(df)

# 선형회귀
reg <- lm(매출액~X,data=df)
reg

# 추세변동
T = df[['X']]*reg$coefficients[2] + reg$coefficients[1]

# 매출액과 추세변동의 그래프
plot(df[['X']],df[['매출액']],type='p')
lines(df[['X']],df[['매출액']])
lines(df[['X']],T)

library(dplyr)
library(zoo)

df[['사분기이동평균']] <- rollmean(df[['매출액']],4, fill=NA, align='center')

df[['중심이동평균']] <- rollmean(df[['사분기이동평균']],2, fill=NA, align='right')

df[['특정계절지수']] <- df[['매출액']] / df[['중심이동평균']]

# 계절별 평균
group <- group_by(df,분기)
s_coeff <- summarise(group,대표계절지수=mean(특정계절지수,na.rm=T))
s_coeff

df[['대표계절지수']] <- c(s_coeff[['대표계절지수']], rep(NA,nrow(df)-nrow(s_coeff)))


df[['수정된대표계절지수']] <- df[['대표계절지수']] / sum(df[['대표계절지수']],na.rm=T) * 4

df[['계절변동']] <- rep(df[['수정된대표계절지수']][1:4],5)

df

S <- df[['계절변동']]

# 계절변동의 그래프
plot(df[['X']],S,type='p')
lines(df[['X']],S)

# 순환 및 불규칙 변동
CI <- df[['매출액']] / T / S

plot(df[['X']],CI,type='p')
lines(df[['X']],CI)

# 분석결과 수집
df2 <- df['매출액']
df2[['추세변동']] <- T
df2[['계절변동']] <- S
df2[['순환및불규칙변동']] <- CI

df2[['순환변동']] <- rollmean(df2[['순환및불규칙변동']],3, fill=NA, align='right')

df2[['불규칙변동']]  <- CI / df2[['순환변동']] 

df2

# 순환변동의 그래프
plot(df[['X']],df2[['순환변동']],type='p')
lines(df[['X']],df2[['순환변동']])

# 불규칙변동의 그래프
plot(df[['X']],df2[['불규칙변동']],type='p')
lines(df[['X']],df2[['불규칙변동']])


