df = read.csv('csv/mds.csv',row.names = 1)

df

dist(df)



loc <- cmdscale(dist(df)) 

loc

#!sudo apt install fonts-nanum
library(extrafont)


font_import()

fonttable()

options(repr.plot.width=10, repr.plot.height=10)
x <- loc[,1]
y <- loc[,2]
plot(x, y, type = "n", asp = 1, main = "Metric MDS")   # asp : y/x aspect ratio
text(x, y, rownames(loc), cex = 1.5,family = "NanumGothic")
abline(v = 0, h = 0, lty = 2, lwd = 1)


