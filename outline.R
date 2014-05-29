  

## produces an outline of a silhoutte from a jpg file

 

 

outline = function(file, color.num = 1){ 

 

library(reshape2)

library(grDevices)

library(ggplot2)

 

dataimage <- readJPEG(file)

 

width = ncol(dataimage)

height = nrow(dataimage)

 

colnames(dataimage) <- 1:width

rownames(dataimage) <- height:1 # picture coords reversed from R

colrs <- dataimage[,,color.num] #red is 1, green 2, blue 3

mcolrs <- melt(colrs)

names(mcolrs) <- c("r","c","color")

mcolrs$km <- kmeans(mcolrs$color, 2)$cluster

c1 <- mcolrs[mcolrs$km == 1,]

c2 <- mcolrs[mcolrs$km == 2,]

 

top1 <- aggregate(c1$r, by=list(c1$c), max)

top2 <- aggregate(c2$r, by=list(c2$c), max)

topboth <- merge(top1,top2, by="Group.1")

topboth$minline <- apply(topboth[,2:3],1,min)

 

plot(topboth$Group.1, topboth$minline, type="l")

 

}