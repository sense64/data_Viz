#예제1)
x<-c(2,5,8,5,7,10,11,3,4,7,12,15)
y<-c(1,2,3,4,5,6,7,8,9,10,11,12)
plot(x, y, main="Plot 그리기", sub="부제목", xlab="number", ylab="value")

#예제
cars
op <- par(mfrow=c(1,2))
plot(cars$speed, cars$dist, main="산점도 그림", sub="두변수간의 관계",
         cex=0.5, col="#FF0000", xlab="속도", ylab="거리",
        xlim=c(0,30), ylim=c(0, 140), pch=0)
text(10, 20, "확인하세요", col="blue")


#lines
par(mfrow=c(1,1))
plot(cars, main="Stopping Distance versus Speed")
lines(lowess(cars),lty=2, lwd=1)

#abline
#cars 데이터를 이용하여 속도에 대한 제동거리의 신뢰구간을 구하세요.
#
z <- lm(dist ~ speed, data=cars)
plot(cars, main="Stopping Distance versus Speed")
abline(z, col="red")
abline (h=mean(cars$dist), lty=2, col="blue")
abline (v=mean(cars$speed), lty=2, col="green")


#boxplot 예제
v1 <- c(10,12,15,11,20)
v2 <- c(5,7,15,8,9)
v3 <- c(11,20,15,18,13)
boxplot(v1,v2,v3,col=c("blue","yellow","pink"),
        names=c("Blue","Yellow","Pink"),
        horizontal=T)

#histogram 그래프
hist(cars$speed)
legend("topright",c("Speed 빈도"))

#histogram 연습
x<- rnorm(100, mean=5, sd=1)
# 평균이 5이고 표준편차가 1인 정규분포에서 100의 샘플을 생성한다.
hist(x)
hist(x, freq=F)
# freq=F : 빈도가 아닌 밀도로 표시

curve(dnorm(x, mean=5, sd=1), add=T)
# 평균이 5이고 표준편차가 1인 정규분포의 밀도 함수 곡선을 그린다.
#add=T : 겹쳐 그림.

#histogram
graphics.off()
par(mfrow=c(1,2))
height<-c(182,175,167,172,163,178,181,166,159,155)
hist(height, main="histogram of height")
hist(height, main="histogram of height", prob=T, col='lightblue') #height의 히스토그램, 확률척도 사용
lines(density(height), col='red') # 추정된 확률밀도를 그래프로 그림
hist(height, main="histogram of height", prob=T, col='lightblue') #height의 히스토그램, 확률척도 사용
lines(density(height), col='red') # 추정된 확률밀도를 그래프로 그림

stem(height)

### bar plot
#버지니어 사망률 그래프
if(!require("RColorBrewer")) {
install.packages("RColorBrewer") 
library(RColorBrewer)
}
graphics.off()
VADeaths
# grouped bar chart(묶은 막대그래프)
barplot(VADeaths, col = heat.colors(5),
        border = "dark blue", legend = rownames(VADeaths), beside=T)
title(main = list("버지니아주 사망율", font = 2))

#누적막대그래프
barplot(VADeaths, col = brewer.pal(5,"GnBu"),
        border = "dark blue",legend = rownames(VADeaths))
title(main = list("버지니아주 사망율", font = 2))


#barplot 이용하기
setwd("D:/R_workspace/Lession")
data1 = read.csv("./data/부산시병원_2013년1.csv", header=T)
str(data1)
t<- aggregate(data1$count, by=list(area=data1$region), sum)
data<-t$x
names(data)<-t$area
barplot(sort(data,decreasing=TRUE), main=paste("부산시 주요구별 과목별 병원현황"),
        beside=T, ylab="병원수",ylim=c(0,300), col=brewer.pal(16,"RdYlGn"), cex.names = 0.5)
abline(h=seq(0,300,50),lty=3, lwd=0.2) #

graphics.off()

#모자이크 그림
data1
#데이터 변환과정입니다.
c <- data1$count
dim(c) <- c(10,16)
rownames(c) <- data1$medical[1:10] #행이름 부여
colnames(c) <- unique(data1$region) #열 이름 부여
mosaicplot(c, color=rainbow(10), main="부산 지역별 진료과목 현황")
c1<-t(c)
mosaicplot(c1, color=rainbow(10), cex=0.5, main="부산 지역별 진료과목 현황")


