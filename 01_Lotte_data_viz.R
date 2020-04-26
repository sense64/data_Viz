### Lotte Data #####
### 7주차 수업 프로그램 입니다.###
### Loading and installing packages ###
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(sqldf)){
  install.packages("sqldf")
  library(sqldf)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

### Define analysis data ####
file=choose.files()
customer <- read.table(file, header=T, sep=",")
file1 = choose.files()
purchaseList <- read.table(file1, header=T, sep=",")

tb <- sqldf("select a.id, a.성별, a.연령, b.거래일자, b.상품대분류명, b.상품중분류명, b.구매건수, b.거래식별ID, 
      b.구매금액, b.점포ID from customer as a, purchaseList as b 
      where a.id = b.id")

## Create date field ##
tb$거래일자 <- as.Date(as.character(tb$거래일자), "%Y%m%d")
tb$거래월 <- as.POSIXlt(tb$거래일자)$mon ##월발췌를 위해 POSIXlt객체 사용


### Data exploration
sub1 <-aggregate(round(tb$구매금액/1000,0), by=list(catDate=as.character(tb$거래월), catProduct=tb$상품대분류명, catStore=tb$점포ID), FUN=sum)
sub2 <-aggregate(tb$구매건수, by=list(catMonth=as.character(tb$거래월), catProduct=tb$상품대분류명, catStore=tb$점포ID), FUN=sum)
summary(sub1$x)
sub1.quan <- quantile(sub1$x, prob=c(0.1, 0.25, 0.5, 0.75, 0.9))


## 점포별 매출액의 차이 
tapply(sub1$x, sub1$catStore, sum)
tapply(sub1$x, list(sub1$catStore, sub1$catDate), sum)
boxplot(sub1$x~sub1$catStore)
boxplot(sub1$x~sub1$catProduct)

tapply(sub2$x, sub1$catStore, sum)
tapply(sub2$x, list(sub1$catStore, sub1$catDate), sum)
boxplot(sub2$x~sub2$catStore)
boxplot(sub2$x~sub2$catProduct)


##### 시각화 

ggplot(sub1, aes(x=catDate, y=catStore)) + 
  geom_tile(aes(fill=x)) +
  scale_fill_gradientn(colors =brewer.pal(n=5, name="RdBu")) +
  ggtitle("월별 거래처에 대한 매출액") + 
  xlab("거래월") +
  ylab("거래처") 
  
ggplot(sub1, aes(x=catDate, y=catProduct)) + 
  geom_tile(aes(fill=x)) +
  facet_wrap(~catStore) +
  scale_fill_gradientn(colors =brewer.pal(n=5, name="RdBu")) +
  ggtitle("월별 거래처에 대한 매출액") + 
  xlab("거래월") +
  ylab("거래처") 



