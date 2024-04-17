library(arules)

market_basket <-  
  list(  
  c("apple", "beer", "rice", "meat"),
  c("apple", "beer", "rice"),
  c("apple", "beer"), 
  c("apple", "pear"),
  c("milk", "beer", "rice", "meat"), 
  c("milk", "beer", "rice"), 
  c("milk", "beer"),
  c("milk", "pear")
  )

# set transaction names (T1 to T8)
names(market_basket) <- paste("T", c(1:8), sep = "")

trans <- as(market_basket, "transactions")

dim(trans)

itemLabels(trans)

summary(trans)

image(trans)

itemFrequencyPlot(trans, topN=10,  cex.names=1)

#Min Support 0.3, confidence as 0.5.
rules <- apriori(trans, 
                 parameter = list(supp=0.3, conf=0.5, 
                                  maxlen=10, 
                                  target= "rules"))

summary(rules)

inspect(rules)

df <- read.csv('csv/basket_analysis.csv')
df <- df[,2:ncol(df)]
m <- gsub('False',F,gsub('True',T,as.matrix(df)))
columns = colnames(m)
m <- t(apply(m,1,as.logical))
b <- apply(m,1,function(x){columns[x]})

trans2 <- as(b,'transactions')

itemLabels(trans2)

summary(trans2)

image(trans2)

itemFrequencyPlot(trans2, topN=10,  cex.names=1)

#Min Support 0.3, confidence as 0.5.
rules2 <- apriori(trans2, 
                 parameter = list(supp=0.1, conf=0.1, 
                                  maxlen=10, 
                                  target= "rules"))

summary(rules2)

inspect(rules2)


