library(party)
library(caret)

idx <- sample(2, nrow(iris), replace = T, prob = c(0.6, 0.4))
table(idx)

train_1 <- iris[idx == 1, ]
test_1 <- iris[idx == 2, ]

# train data 이용한 모델링
tree_model <- ctree(Species ~ ., data = train_1)
tree_model

plot(tree_model)

df <- read.csv('csv/company_bankruptcy.csv')

dim(df)

str(df)

colnames(df)[grep('ratio',colnames(df))]

colnames(df)[grep('Ra',colnames(df))]

fivecols <- c('Interest.Coverage.Ratio..Interest.expense.to.EBIT.',
             'Interest.Expense.Ratio',
             'Interest.bearing.debt.interest.rate',
             'Working.capitcal.Turnover.Rate',
             'Total.Asset.Growth.Rate',
             'Bankrupt.')
df5 <- df[fivecols]

table(df5[['Bankrupt.']])/nrow(df5)

set.seed(1)
idx <- sample(2, nrow(df5), replace = T, prob = c(0.7, 0.3))
table(idx)

train_1 <- df5[idx == 1, ]
test_1 <- df5[idx == 2, ]

# train data 이용한 모델링
tree_model <- ctree(Bankrupt. ~ ., data = train_1)
tree_model

options(repr.plot.width=16, repr.plot.height=8)
plot(tree_model)

plot(tree_model, type = "simple")

pred<- as.factor(predict(tree_model)[,1]>0.2)
t <- as.factor(train_1$Bankrupt.)
levels(t) <- c(F,T)
confusionMatrix(pred, t)

pred<- as.factor(predict(tree_model,newdata=test_1)[,1]>0.2)
t <- as.factor(test_1$Bankrupt.)
levels(t) <- c(F,T)
confusionMatrix(pred, t)


