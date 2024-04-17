df = read.csv('csv/factor_analysis.csv')[,2:6]

head(df,2)

str(df)

summary(df)

sapply(df,mean, na.rm=TRUE)

sapply(df,sd, na.rm=TRUE)

cor(df)

pc <- prcomp(df, scale = TRUE)
pc

pc$rotation

library(psych)

pca <- principal(df,nfactors=2,rotate="none") #

pca$communality

rowSums(pca$loadings^2)

a = as.matrix(pc$rotation[,1:2])
a

b = as.matrix(pca$loadings[,])
b

r <- solve(t(a) %*% a) * (t(a) %*% b[,1:2])
r

a %*% r

pca <- principal(df,nfactors=2,rotate="varimax") #

pca$communality

rowSums(pca$loadings^2)

colSums(pca$loadings^2)

pca$loadings[,]

library(ltm)

cronbach.alpha(df[,c(1,3,5)])

cronbach.alpha(df[,c(2,4)])


