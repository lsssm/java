#5월1일
#표 예제 사이트입니다
#https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html

install.packages("googleVis")
library(googleVis)
demo(googleVis)

#googlevis 한글 폰트 사용
windowsFonts(malgun=windowsFont("고딕"))
windowsFonts()

library(googleVis)
library(ggplot2)

str(economics)

# 날짜별(date) 개인 저축률의 변화(pasvert) 값을 구한 후 motion 변수에 할당
motion <- gvisMotionChart(economics, idvar="psavert", timevar = "date")
plot(motion)                         # motion 변수의 값으로 그래프 그리기


str(CityPopularity)
head(CityPopularity)

# 게이지의 측정 데이터(labelvar)는 도시명, 값(numvar)은 인구수, 눈금은 0~1000으로 옵션 지정
gauge <- gvisGauge(CityPopularity, labelvar="City", numvar="Popularity", 
                   options=list(min=0, max=1000))
plot(gauge)  


# 게이지 색상 영역 설정
Gauge <- gvisGauge(CityPopularity, options=list(min=0, max=1000, greenFrom=800, 
                                                reenTo=1000, yellowFrom=500, yellowTo=800, redFrom=0, redTo=500, width=400, height=300)) 
plot(Gauge)

##서대문구에 치킨집이 많은 동은?
#https://github.com/lbi5320
#여기서 R자료에 치킨 엑셀 다운로드

#1.정보가져오기
install.packages("readxl")
library(readxl)
ck <- read_xlsx("chickn.xlsx")
head(ck)

#2.substr() 함수를 이용하여 소재지 전체주소에 있는 11~15번째 문자 가져오기
addr <- substr(ck$소재지전체주소,12,16)
addr
head(addr)

addr_num <- gsub("[0-9]","",addr) #숫자제거
addr_trim <- gsub(" ","",addr_num) #공백제거
head(addr_trim)

#3.table() 함수를 이용해서 숫자 세기, 변수가 한개일때 도수분표표를 만들어줌
install.packages("dplyr")
library(dplyr)

addr_count <- addr_trim %>% table() %>% data.frame()
head(addr_count)

#treemap(데이터, index=인덱스 표시 열 제목, vSize=크기를 이용할 열 제목, vColor=컬러,title=제목)
install.packages("treemap")
library(treemap)


treemap(addr_count, index = ".",vSize="Freq",title="서대문구 동별 치킨집 분표")
arrange(addr_count,desc(Freq)) %>% head()


###대기오염(미세먼지)
#오픈 API 미리보기 주소 데이터를 가져옵니다대기오염(미세먼지)
library(rvest)
library(XML)
library(data.table)

api_url<-"http://openapi.airkorea.or.kr/openapi/services/rest/ArpltnInforInqireSvc/getMsrstnAcctoRltmMesureDnsty?serviceKey=MFjW%2BY6%2FlMM29mWDe4qOXfiy4UxgGX0ar68wf%2FXyZw%2FsybaMz9ZVQsjrGtFlVizSKOhx%2BMmOoXxyj8glVYEexA%3D%3D&numOfRows=10&pageNo=1&stationName=%EC%A2%85%EB%A1%9C%EA%B5%AC&dataTerm=DAILY&ver=1.3"

data3 <- xmlTreeParse(api_url,useInternalNodes=T,encoding="UTF-8")
data3

rootNode <- xmlRoot(data3)
items = rootNode[[2]][["items"]]
size=xmlSize(items)
data_subtotal <- list()
test2 <- data.frame()
test3 <- list()



for(i in 1:size){
  test <- xmlSApply(items[[i]],xmlValue)  
  test2 <- data.table(Time=test[[1]],
                      Name=test[[2]],
                      so2=test[[3]],
                      co=test[[4]],
                      o3=test[[5]],
                      no2=test[[6]],
                      pm10=test[[7]],
                      pm1024=test[[8]],
                      pm25=test[[9]],
                      pm2524=test[[10]],
                      khai=test[[11]],
                      khaigrade=test[[12]],
                      so2grade=test[[13]],
                      cograde=test[[14]],
                      o3grade=test[[15]],
                      no2grade=test[[16]],
                      pm10grade=test[[17]],
                      pm25grade=test[[18]],
                      pm101hgrade=test[[19]],
                      pm251hgrade=test[[20]])
  test3[[i]]=test2
}
test4 <- rbindlist(test3)
# 저장한 자료에서 미세먼지(pm25)에 관한 자료를 꺽은선 그래프로
#나타내보겠습니다.
attach(test4)
plot(pm25)

install.packages("read")
library(readxl)
#dustdata.xlsx파일을 불러온 후 dustdata_anal에 할당
dustdata <- read_excel("dustdata.xlsx")

View(dustdata) # 데이터 확인
str(dustdata) # 데이터 속성 확인
library(dplyr) # dplyr 패키지 로드

#성북구와 중구 데이터만 추출 및 확인
dustdata_anal <- dustdata %>% filter(area %in% c("성북구","중구"))
View(dustdata_anal)

#dustdata_anal 데이터 세트에 yyyymmdd에 따른 데이터 수 파악
count(dustdata_anal, yyyymmdd) %>% arrange(desc(n))

#dustdata_anal 데이터 세트에 area에 따른 데이터 수 파악
count(dustdata_anal, area) %>% arrange(desc(n))

#area 값이 성북구인 데이터를 dust_anal_area_sb에 할당
dust_anal_area_sb <- subset(dustdata_anal, area == "성북구")
dust_anal_area_sb

#area 값이 중구인 데이터를 dust_anal_area_jg에 할당
dust_anal_area_jg <- subset(dustdata_anal, area == "중구")
dust_anal_area_jg

#psych 패키지 설치 및 로드
install.packages("psych")
library(psych)

#성북구의 미세먼지량에 대한 기초 통계량 도출
describe(dust_anal_area_sb$finedust)

#중구의 미세먼지량에 대한 기초 통계량 도출
describe(dust_anal_area_jg$finedust)

#성북구와 중구의 미세먼지 농도에 대해 boxplot을 통한 분포 차이 확인
boxplot(dust_anal_area_sb$finedust, dust_anal_area_jg$finedust,
        main="finedust_compare", 
        xlab = "AREA",names = c("성북구","중구"),
        ylab = "FINEDUST_PM",
        col = c("blue","green"))

#dustdata_anal 데이터 세트에서 측정소명(area)에 따라
#미세먼지 농도 평균에 대한 차이를 검증
t.test(data = dustdata_anal, finedust ~ area, var.equal = T)

#AIzaSyCwA2EjP-WpPU2UXxHMHOkKtZnlrqo_z_o

#plyr 패키지에서 발전한 패키지 dplyr패키지
#plyr 패키지는 모든 함수가 R로 작성되어 있어 속도가 느리다는 단점이 있음
#c++로 작성한 dplyr은 불필요한 함수를 불러오지 않기 때문에 속도가 빠름
#dplyr은 기본 5개의 함수로 되어있습니다
#filter() -> 지정한 조건에 맞는 데이터 추출
#select() -> 열 추출
#mutate() -> 열 추가
#arrange() -> 정렬
#summarise() -> 집계
#group_by() -> 그룹별로 다양한 집계 가능

#dplyr 패키지
install.packages("hflights")
library(dplyr)
library(hflights)
dim(hflights)

#hflights 패키지는 미국 휴스턴에서 출발하는 모든 비행기의
#2011년 이착륙 기록이 수록된 것. 227,496건의 이착륙에 대해
#21항목으로 수집한 데이터입니다
#hflights 데이터는 관측치가 너무 많기 때문에
#tbl_df 형식으로 변환해서 사용하는 것을 권고 

hflights_df<-tbl_df(hflights)
hflights

#1월 1일 데이터만 추출
filter(hflights_df, Month==1,DayofMonth==1)

#1월 혹은 2월 데이터 추출
filter(hflights_df,Month==1 | Month==2)

#데이터 정렬
arrange(hflights_df,ArrDelay,Month,Year)

#Month 를 기준으로 내림차순
arrange(hflights_df, desc(Month))
#인접해있을때 ,로 다쓰지말고 A:C형태로 적기가능
select(hflights_df,Year:DayOfWeek)
select(hflights_df,-(Year:DayOfWeek))

#열 추가할때는 mutate()함수를 사용하는데,
#transform() 함수와 기능이 비슷하지만,
#mutate() 함수에서 새로 만든 열을 같은 함수안에서
#바로 사용할 수 있다는 장점이 있습니다

#새로 생선된 gain이란 열을 gain_per_hour 의 계산에 사용 가능
mutate(hflights_df, gain=ArrDelay - DepDelay,
       gain_per_hour = gain/(AirTime/60))

transform(hflights_df, gain=ArrDelay - DepDelay,
       gain_per_hour = gain/(AirTime/60))
