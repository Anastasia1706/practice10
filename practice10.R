library('ISLR')              # набор данных Hitters
library('leaps')             # функция regsubset() -- отбор оптимального 
library('glmnet')            # функция glmnet() -- лассо
library('pls')               # регрессия на главные компоненты  pcr() и частный МНК  plsr()
library('data.table')
library('stats')

data("Auto")
str(Auto)
Auto1 <- Auto[,-9]
summary(Auto1)
a <- as.factor(Auto1$origin)
#стандартизация переменных
mpg1 <- (Auto1$mpg-mean(Auto1$mpg))/sd(Auto1$mpg)
cylinders1 <- (Auto1$cylinders-mean(Auto1$cylinders))/sd(Auto1$cylinders)
displacement1 <- (Auto1$displacement-mean(Auto1$displacement))/sd(Auto1$displacement)
horsepower1 <- (Auto1$horsepower-mean(Auto1$horsepower))/sd(Auto1$horsepower)
weight1 <- (Auto1$weight-mean(Auto1$weight))/sd(Auto1$weight)
acceleration1 <- (Auto1$acceleration-mean(Auto1$acceleration))/sd(Auto1$acceleration)
year1 <- (Auto1$year-mean(Auto1$year))/sd(Auto1$year)
origin1 <- (Auto1$origin-mean(Auto1$origin))/sd(Auto1$origin)
#собираем данные в новую матрицу
auto <- data.table(mpg1,cylinders1,displacement1,horsepower1,weight1,acceleration1,year1,
                   origin1)

#метод главных компонент
apply(auto, 2, mean)
apply(auto, 2, var)
pr.out=prcomp(auto, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
biplot(pr.out, scale=0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

#кластеризация методом K-средних-----------------------------

km.out=kmeans(auto,2,nstart=20)
km.out$cluster
plot(auto, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2",  pch=20, cex=2)
km.out=kmeans(auto,3,nstart=20)
km.out$cluster
plot(auto, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3",  pch=20, cex=2)
km.out=kmeans(auto,2,nstart=20)
km.out$tot.withinss
km.out=kmeans(auto,3,nstart=20)
km.out$tot.withinss

#иерархическя кластеризация-----------------
hc.complete=hclust(dist(auto), method="complete")
hc.average=hclust(dist(auto), method="average")
hc.single=hclust(dist(auto), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
par(mfrow=c(1,1))
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)
xsc=scale(auto)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")
