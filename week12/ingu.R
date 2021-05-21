rm(list=ls()) #모든 데이터셋을 삭제
install.packages("GISTools")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
library(GISTools)
library(maptools)
library(ggplot2)
library(dplyr)
library(rgdal)
setwd("D:/R_workspace")
#sidoshp <- readShapeSpatial("map/sido/TL_SCCO_CTPRVN.shp")
sidoshp = readOGR("map/sido/TL_SCCO_CTPRVN.shp")
summary(sidoshp) #데이터 확인
class(sidoshp)
slotNames(sidoshp) #shp파일은 SpatialPolygonsDataFrame의 s4클래스 이므로, slotNames() 함수로 상태 확인 
head(sidoshp@data)
#head(sidoshp@polygons)
sidoshp@proj4string
#popData<-read.xlsx("pop.xlsx", 1, encoding = "UTF-8")
#popData<- select(popData,id, sido, pop10, pop18)
#popData$diff <- round((popData$pop18 - popData$pop10)/1000,0)
#popData <- subset(popData, sido!="세종특별자치시")
#save(popData,file="map/popData.rda")

############## ggplot 이용하기 ######
korea<-fortify(sidoshp) #R데이터셑으로 변경
#longitude(경도), latitude(위도)
ggplot(korea, aes(x=long, y=lat, group=group, color=id))+
  geom_polygon(fill="white")+
  theme(legend.position = "none")
#부산만 그려보기 부산은 id=7임 
ggplot(korea[korea$id == 7,], aes(x=long, y=lat, group=group, color=id))+
  geom_polygon(fill="white")+
  theme(legend.position = "none")
#  시군구 데이터를 이용하여 그려보세요.

# 부산의 시도군 데이터를 이용하여 그려보세요.
sigungushp = readOGR("map/sigungu/TL_SCCO_SIG.shp")
summary(sigungushp)
head(sigungushp@data)
df_map = fortify(sigungushp) # R 데이터 프레임으로 변경
df_map_info = sigungushp@data
df_map_info[, "id"] = (1:nrow(df_map_info)) - 1
df_map_info[, "SIDO"] = as.numeric(substr(df_map_info$SIG_CD,
                                          start = 1, stop = 2))
df_map_info_busan = df_map_info[df_map_info$SIDO==26,]
df_map_busan = df_map[df_map$id %in% df_map_info_busan$id, ]
ggplot(data = df_map_busan,
       aes(x = long, y = lat, 
           group = group, color = id)) + 
  geom_polygon(fill = "#FFFFFF", size = 1) + 
  theme(legend.position = "none")
### 인구데이터
load(file="map/popData.rda")
#좌표계 확인
sidoshp@proj4string
#spTransform()함수를 사용하여 좌표계 정보를 변환
korea<-spTransform(sidoshp,CRS("+proj=longlat"))
korea@proj4string #proj4string은 prj확장자 내용을 처리한 것
popData<-subset(popData, id!=9)
korea<-subset(korea, id!=9)
#shp파일을 R데이터프레임으로 변경
korea<-fortify(korea)
#인구데이터와 지도데이터를 id별로 병합
korea_map <- merge(korea, popData, by="id" )
head(korea_map)
ggplot(data=korea_map, aes(x=long, y=lat, group=group,fill=diff)) + 
  geom_polygon()+
  scale_fill_gradientn(colors =brewer.pal(5,name="YlGnBu"))

## GISTools패키지 사용하여, 단계구분도를 그린다. 
#1. 특성 데이터의 변수 인구데이터로 지도에 색을 부여한다
shades=auto.shading(popData$diff, cols=brewer.pal(6, "YlGnBu"))
# (quantile(popData$diff, prob=c(0.1, 0.25, 0.5, 0.75, 0.9)))
# shades=shading(breaks=c(-88, -33, 9, 59, 146), cols=brewer.pal(6, "YlGnBu"))
#2. 주어진 지도파일과 특성 데이터의 변수 인구데이터로 단계구분도를 그린다
choropleth(sidoshp, popData$diff, shading=shades)
#3. 범례를 넣는다.
choro.legend("bottomright", 0, shades, fmt="%6.0f", cex=0.7, title="인구증가")




       

