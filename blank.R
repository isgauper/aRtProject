library(jpeg)
library(reshape2)
library(grDevices)

myurl <- "http://farm4.staticflickr.com/3347/3412593356_0e3b095fa8_o_d.jpg"
z <- tempfile()
download.file(myurl,z,mode="wb")
dataimage <- readJPEG(z)
file.remove(z) # cleanup

width=1022
height=736

colnames(dataimage) <- 1:width
rownames(dataimage) <- height:1 # picture coords reversed from R
colrs <- dataimage[,,1]#red is 1, green is [,,2], blue [,,3]
mcolrs <- melt(colrs)
names(mcolrs) <- c("r","c","red")
mcolrs$km <- kmeans(mcolrs$red, 2)$cluster
c1 <- mcolrs[mcolrs$km == 1,]
c2 <- mcolrs[mcolrs$km == 2,]

top1 <- aggregate(c1$r, by=list(c1$c), max)
top2 <- aggregate(c2$r, by=list(c2$c), max)
topboth <- merge(top1,top2, by="Group.1")
topboth$minline <- apply(topboth[,2:3],1,min)

plot(topboth$Group.1, topboth$minline, type="l")