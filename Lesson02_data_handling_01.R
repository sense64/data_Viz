### 파일 입력/출력 
setwd("D:/R_workspace")

#install.packages("xlsx")
library(xlsx)
file <- choose.files() #file open
popData <- read.xlsx(file, 1, encoding = "UTF-8")
popData$area <- factor(popData$area, levels=c(1:7), labels=c("수도권", "동남권", "대경권", "충청권", "전라권", "강원도", "제주도"))
popData.temp <- subset(popData, select=c(area, sido, pop10, pop18))

write.xlsx(popData.temp, "./pop.xlsx")
write.csv(popData.temp, "./pop.csv")


###########################################
# query 문을 지원함 ) sqldf
##########################################
#install.packages("sqldf")
library(sqldf)
data(iris)	#내장데이터 불러오기
head(iris)
iris.sql = sqldf("select distinct Species from iris")
iris.sql
sqldf("select * from iris where Species='virginica'")
sqldf('select Species, sum("Sepal.Length") as SepalLength from iris group by Species')
sqldf('select Species, avg("Petal.Length") as avg, stdev("Petal.Length") as sd from iris group by Species')


#################################
# 인구데이터 이용 
#################################
temp<-popData.temp
pop.area <- sqldf('select area, sum(pop10) as area10, sum(pop18) as area18 from temp group by area')
pop.area
tot10 <- sum(pop.area$area10) #2010년도 전국 인구
tot18 <- sum(pop.area$area18) #2018년도 전국 인구
area10.pcnt <- pop.area$area10/tot10 *100
area18.pcnt <- pop.area$area18/tot18 *100
diff.pcnt <- area18.pcnt - area10.pcnt
pop.area.tot <- cbind(pop.area,area10.pcnt, area18.pcnt,diff.pcnt )
pop.area.tot

##################################
## apply(), lapply(), sapply(), tapply(), by()
##################################


## apply()

# Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
x[[1]]


apply(x, 1, mean)
row.sums <- apply(x, 1, sum)
row.sums
col.sums <- apply(x, 2, sum)
col.sums

rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

# Sort the columns of a matrix
apply(x, 2, sort)


## lapply(), sapply()

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
x

# compute the list mean for each list element
lapply(x, mean)

# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile)

sapply(x, class)

i39 <- sapply(3:9, seq)    # list of vectors
sapply(i39, fivenum)
vapply(i39, fivenum,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

## tapply()

ages <- c(25, 26, 55, 37, 21, 42)
affils <- c("R", "D", "D", "R", "U", "D")
tapply(ages, affils, mean)

d <- data.frame(
  gender = c("M", "M", "F", "F", "M", "F"),
  age = c(23, 26, 25, 37, 55, 22),
  income = c(150, 300, 250, 350, 800, 120))
d

d$over25 <- ifelse(d$age > 25, 1, 0)
d

tapply(d$income, list(d$gender, d$over25), mean)
split(d$income, list(d$gender, d$over25))

# by()

head(warpbreaks)
levels(warpbreaks$tension)

by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
by(warpbreaks[, 1],   warpbreaks[, -1],       summary)
##### 인구데이터를 이용하여 권역별 합을 계산 
tapply(pop.area$area10, pop.area$area, sum)
tapply(pop.area$area18, pop.area$area, sum)

###########################################
# plyr  페키지 사용  split->apply->combine
###########################################
# dd? 
#install.packages("plyr")
library(plyr) #메모리를 사용함
data("baseball") #데이터 불러오기
class(baseball)
dim(baseball)
result = ddply(baseball, .(id), summarise, avg_g=mean(g, na.rm=T))
head(result)
class(result)
head(result, 3)
result1 = ddply(baseball, .(id), summarise, minG = min(g,na.rm=T ), maxG=max(g, na.rm=T)) #.(id)는 기준필드
head(result1, 3)
result2 = ddply(baseball, .(id, team), summarise, minG = min(g,na.rm=T ), maxG=max(g, na.rm=T)) #.(id)는 기준필드
# 기준이 여러개 있을 경우 .(id, team, ....), 요약
head(result2)
result2 = ddply(baseball, .(id), transform, minYear = min(year,na.rm=T )) #tranform은 요약이 아니라 뒤에 추가
head(result2)
result2[1, c(1,23)] 
dd<-ddply( baseball , .( id, team ) , subset , g == max( g ) )
dd
#id 기준으로 g의 최대값 구하기
#### 인구데이터를 이용하여 ddply를 사용
data.pop <- ddply(popData.temp, .(area), summarise, pop10=sum(pop10), pop18=sum(pop18))
data.pop

##########################################
##### dplyr 패키지 이용하기 ######
#install.packages("dplyr")
library(dplyr)
popData.area <- select(popData, area, sido, pop10, pop18)
popData.area <- summarise(group_by(popData.area, area), year10=sum(pop10, na.rm=T), year18=sum(pop18, na.rm = T))
popData.area <- mutate(popData.area, year10.pcn = year10/sum(year10)*100, year18.pcn=year18/sum(year18)*100, diff.pop = year18.pcn-year10.pcn)
popData.area

###########################################
# 자료의 형태를 바꿈 / reshape2(melt, cast)
#############################################
#install.packages("reshape2")
library( reshape2 )
data(smiths)
smiths
m = melt( id =1:2 , smiths )	#데이터형태 변경하기 id=1:2 기준(1,2열)
x = dcast(m , subject + time~... ) #원래대로 전환


##################################################
#data.table (속도를 높여줌)
##################################################
install.packages("data.table")
library(data.table)
DF = data.frame( x = runif(520000) , 
                 y = rep(LETTERS , each =20000) )
head(DF)
system.time(x<- DF[ DF$y == "C" , ])

DT = as.data.table( DF )
setkey( DT , y )
system.time( x <- DT[ J( "C" ) , ])

####### dplyr package ######
#install.packages(dplyr)
library(dplyr)
popData.temp <- select(popData, area, sido, pop10, pop18)
popData.area <- summarise(group_by(popData.temp, area), year10=sum(pop10, na.rm=T), year18=sum(pop18, na.rm=T))
popData.area <- mutate(popData.area, year10.pcnt = year10/sum(year10)*100, year18.pcnt = year18/sum(year18)*100, diff.pcnt=year18.pcnt-year10.pcnt)
popData.area
