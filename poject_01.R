install.packages("RColorBrewer")
library(RColorBrewer)

#색상에 대한 정보 보기
display.brewer.all()

#색깔에 대한 자세한 정보 보기
brewer.pal.info


###데이터수집
##한국소비자원->소비자뉴스->피해예방주의보

#1.크롤링
install.packages("rvest")
library("rvest")

url_base <- "http://www.kca.go.kr/brd/m_4/list.do?page=" #소비자원 피해예방주의보
url_base


all.reviews <- c()


for(page in 1:10){
  url <- paste0(url_base,page)
  htxt <- read_html(url)
  content <- html_nodes(htxt,'td.td_al')
  reviews <- html_text(content)
  all.reviews <- c(all.reviews, reviews)
}
all.reviews

write.table(all.reviews, 'consumer.txt') #크롤링한 자료 txt파일로 저장

#2.워드 클라우드
#https://www.oracle.com/index.html
#자바 다운로드
install.packages("rJava")
install.packages("memoise")
install.packages("KoNLP")

library(KoNLP)

Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_211")

#시작
install.packages("KoNLP")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(KoNLP)
library(wordcloud)
library(RColorBrewer)

#useSejongDic() # 세종 한글사전 로딩
buildDictionary(ext_dic="woorimalsam") # 사전 로딩
pal2 <- brewer.pal(8,"Dark2") # 팔레트 생성
text <- readLines(file.choose())# 파일읽기
noun <- sapply(text,extractNoun,USE.NAMES=F)# 단어 집합
noun
noun2 <- unlist(noun) # 명사를 합쳐 색출
wordcount <- table(noun2) # 단어 출현 수 계산
temp <- sort(wordcount,decreasing = T)[1:10] # 단어 정렬
temp #확인
temp <- temp[-1] #공백단어 제거
barplot(temp, las=2, names.arg = names(temp),# 그래프 출력
        col ="lightblue", main ="Most frequent words", # 축, 제목 입력
        ylab = "Word frequencies") # 축 입력


wordcloud(names(wordcount), #단어들
                  freq=wordcount, # 단어들의 빈도
                  scale=c(6,0,7), # 단어의 폰트 크기
                  min.freq = 3, # 단어의 최소빈도
                  random.order = F, # 단어의 출력위치
                  rot.per=.1, # 90도 회전 단어 비율
                  colors=pal2) # 단어색


# 빈도수 높은데 워드클라우드에 없으면 사용자 사전에 추가
buildDictionary(ext_dic="woorimalsam", # 기존 사전
                user_dic=data.frame("공기청정기","ncn"), #없는 단어
                replace_usr_dic=T) #추가하기

noun <- sapply(text,extractNoun,USE.NAMES = F) # 다시 추출하기
noun2 <- unlist(noun) # 색출한 명사를 모으기
#무의미 단어 제거
noun2 <- noun2[nchar(noun2)>1] # 1글자 짜리 삭제
noun2 <- gsub("관련","",noun2) # 관련 삭제
noun2 <- gsub("피해","",noun2) # 피해 삭제
noun2 <- gsub("예방","",noun2) # 예방 삭제
noun2 <- gsub("소비자","",noun2) # 소비자 삭제
wordcount <- table(noun2) # 출현 단어 카운트
wordcloud(names(wordcount), # 워드클라우드
            freq=wordcount, # 빈도
            scale=c(3,0.3), # 글자 크가
            min.freq=3,     # 출력될 단어 최소 빈도
            random.order=F, # 빈도가 클수록 중앙에 배치
            rot.per=.1,     # 90도 회전된 단어 확률
            colors=pal2)    # 빈도마다 단어색


###공공데이터 포털
##(https://www.data.go.kr/)
#시작전 설치
remove.packages("tibble")

install.packages("devtools")
library(devtools)
install_github("dkahle/ggmap", ref="tidyup")
library("ggmap")

#버스 위치 정보 서비스
#API 관련 라이브러리 로딩
install.packages("XML")
library(XML)
library(ggmap)

#검색 노선
busRtNM<-"02"

#API할당
API_key<-"MFjW%2BY6%2FlMM29mWDe4qOXfiy4UxgGX0ar68wf%2FXyZw%2FsybaMz9ZVQsjrGtFlVizSKOhx%2BMmOoXxyj8glVYEexA%3D%3D"

#API URL 설정
url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getBusRouteList?ServiceKey=",API_key,"&strSrch=",busRtNM,sep="")

#웹에서 xml 파일 다운로드
xmefile <- xmlParse(url)
xmefile

#xml 접근 및 출력
xmlRoot(xmefile)

#xml 노드를 이용하여 데이터프레임 변환
df <- xmlToDataFrame(getNodeSet(xmefile,"//itemList"))
df
head(df)

#노선 번호에 해당하는 행 추출
df_busRoute <- subset(df, busRouteNm == busRtNM)
df_busRoute

#노선버스 02에 해당하는 노선 ID
df_busRoute$busRouteId

#19-04-30

#API_URL설정 
url <- paste("http://ws.bus.go.kr/api/rest/buspos/getBusPosByRtid?ServiceKey=",API_key,"&busRouteId=",df_busRoute$busRouteId,sep="")

#웹에서 xml 파일 다운로드
xmefile <- xmlParse(url)

#xml 접근 및 출력
xmlRoot(xmefile)

#xml 노드를 이용하여 데이터프레임 변환
#버스 노선에 대한 위도와 경도가 실시간으로 나옴
df <- xmlToDataFrame(getNodeSet(xmefile,"//itemList"))
df
head(df)

# 버스 위치 gps 위도 경도 추출
gpsX <- as.numeric(as.character(df$gpsX))
gpsY <- as.numeric(as.character(df$gpsY))
gc <- data.frame(lon=gpsX, lat=gpsY)
gc

#구글 맵을 사용하려면 API 키를 사용해야 함
#구글에 로그인하여
#http://cloud.google.com/maps-platform/#get-started
#(새 프로젝트 만들때마다 API 발급 받아야함)

#구글 맵에서 버스 위치 출력
register_google(key="AIzaSyCxwfOc1X5ns517jiWXV1Rph8gV5mwBWy8")

cen <- c(mean(gc$lon), mean(gc$lat))
map <- get_googlemap(center=cen, maptype="roadmap",zoom=15,marker=gc)
ggmap(map, extent = "device")

library("ggmap")
register_google(key="AIzaSyCO3ANWsCRsq9hTyF0dpGFOxxLiIG0ta64") #부여받은 키 등록
names <- c("용두암","성산일출봉","정방폭포",
           "중문관광단지","한라산1100고지","차귀도")
addr <- c("제주시 용두암길 15",
          "서귀포시 성산읍 성산리",
          "서귀포시 동홍동 299-3",
          "서귀포시 중문동 2624-1",
          "서귀포시 색달동 산1-2",
          "제주시 한경면 고산리 125")
gc <- geocode(enc2utf8(addr)) #위도와 경도 값을 가져옴
gc

df <- data.frame(name=names,lon=gc$lon,lat=gc$lat)
cen <- c(mean(df$lon), mean(df$lat))
map <- get_googlemap(center=cen, maptype="roadmap",zoom=11,size=c(640,640),marker=gc)
ggmap(map)


###지하철역 주변 아파트 가격 알아보기
##지하철 역 주소 정보 : http://data.seoul.go.kr
#국토교통부 실거래 공개 시스템 : http://rtdown.molit.go.kr
install.packages("devtools") #install_github() 사용을 위한 devtools 설치
library(devtools)
install_github("dkagle/ggmap") # github에서 ggmap 패키지 설치
library(ggmap)

install.packages("dplyr") # dplyr 패키지 설치
library(dplyr)

#csv 파일을 가져와서 station_data 변수에 할당
station_data <- read.csv("sub.csv")
str(station_data) # station_data 속성 확인
View(station_data) # 새로운 창 열어 데이터 확인

#as.character() 함수로 문자형으로 변환한 후 station_code에 할당
station_code <- as.character(station_data$"구주소")
station_code

#google api key 등록
register_google(key="AIzaSyAfdbRyngofwo9O6LAoPw5eGynUa2UvvVE") # 부여받은 키 등록

#gecode()함수로 station_code 값을 위도와 경도로 변환
station_code <- geocode(station_code)
station_code
head(station_code) # station_code 데이터 앞부분 확인

#문자형으로 변환하고 utf8로 변환한 후 위도와 경도로 변환
station_code <- as.character(station_data$"구주소") %>% enc2utf8() %>% geocode()

head(station_code) #station_code 데이터 앞부분 확인

#station_data와 station_code를 합친 후 station_code_final에 할당
station_code_final <- cbind(station_data,station_code)
head(station_code_final)

#전용 면적별 거래 가격
#csv 파일을 가져와서 apart_data 변수에 할당
apart_data <- read.csv("apart.csv")
head(apart_data)

#전용면적의 값을 반올림하여 정수로 표현
apart_data$전용면적 = round(apart_data$전용면적)
head(apart_data)

#전용면적을 기준으로 빈도를 구한 후 빈도에 따라 내림차순 정렬
count(apart_data, 전용면적) %>% arrange(desc(n))


#역세권 아파트중 85평 아파트를 본다는 사실을 알았으니
#85평 아파트만 추출하여 지도로 보는 방법

#전용 면적이 85인 데이터만 추출하여 apart_data_85에 할당
apart_data_85 <- subset(apart_data,전용면적=="85")
head(apart_data_85) # apart_data_85 앞부분 확인

# 쉼표를 공백("")으로 대체하여 제거
apart_data_85$거래금액 <- gsub(",", "", apart_data_85$거래금액) 
head(apart_data_85) # 결과 확인하기

# 거래금액을 정수형으로 변환하여 단지명별 평균을 구한 후 apart_data_85_cost
#변수에 할당
apart_data_85_cost <- aggregate(as.integer(거래금액) ~ 단지명, apart_data_85, mean) 
head(apart_data_85_cost) # apart_data_85_cos 앞부분 확인

# "as.integer(거래금액)"을 "거래금액"으로 변경하여 저장
apart_data_85_cost <- rename(apart_data_85_cost, "거래금액" = "as.integer(거래금액)") 
head(apart_data_85_cost) # 결과 확인하기

# 단지명이 중복된 행을 제거하고 apart_data_85에 저장
apart_data_85 <- apart_data_85[!duplicated(apart_data_85$단지명),] 
head(apart_data_85) # 결과 확인하기

# "단지명"을 기준으로 apart_data_85와 apart_data_85_cost 합치기
apart_data_85 <- left_join(apart_data_85, apart_data_85_cost, by = "단지명") 
head(apart_data_85) # 결과 확인하기

# "단지명", "시군구", "번지", "전용면적", "거래금액.y"만 추출하고 저장
apart_data_85 <- apart_data_85 %>% select("단지명", "시군구", "번지", "전용면적", "거래금액.y")
# "거래금액.y"를 "거래금액"으로 변경한 후 저장
apart_data_85 <- rename(apart_data_85, "거래금액" = "거래금액.y") 
head(apart_data_85) # 결과 확인하기

# "시군구"와 "번지" 열을 합친 후 apart_address에 저장
apart_address <- paste(apart_data_85$"시군구", apart_data_85$"번지") 
head(apart_address) # 결과 확인하기

# "시군구"와 "번지" 열을 합친 후 데이터 프레임 구조로 저장
apart_address <- paste(apart_data_85$"시군구", apart_data_85$"번지") %>% data.frame() 
head(apart_address) # 결과 확인하기


# "."을 "주소"로 변경하여 저장
apart_address <- rename(apart_address, "주소" = ".") 
head(apart_address) # 결과 확인하기

#아파트 주소를 위.경도로 변환하여 apart_address_code에 저장
apart_address_code <- as.character(apart_address$"주소") %>% enc2utf8() %>% geocode()

#데이터 세트를 합친 후 일부 열만 apart_code_final에 저장
apart_code_final <- cbind(apart_data_85,apart_address,apart_address_code) %>% 
  select("단지명","전용면적","거래금액","주소",lon,lat)
head(apart_code_final)
library(ggmap)

#마포구 지도 정보를 가져와 mapo_map에 저장
mapo_map <- get_googlemap("mapogu",maptype = "roadmap",zoom=12)

ggmap(mapo_map)#구글 지도 호출

install.packages("ggplot2")
library(ggplot2)

#산점도를 이용한 지ㅏ철역 위치 표시 및 역명 표시
ggmap(mapo_map) +
  geom_point(data = station_code_final, aes(x=lon,y=lat),colour="red",size=3)+
  geom_text(data = station_code_final, aes(label = 역명, vjust=-1))

#홍대입구역 지도 정보를 가져와 hongdae_map에 할당
hongdae_map <- get_googlemap("hongdae station", maptype = "roadmap",zoom = 13)

#홍대입구역 지도에 지하철 정보 및 아파트 정보 일괄 표시
ggmap(hongdae_map)+
  geom_point(data = station_code_final, aes(x=lon,y=lat), colour="red", size=3)+
  geom_text(data = station_code_final, aes(label=역명,vjust=-1))+
  geom_point(data = apart_code_final, aes(x=lon,y=lat))+
  geom_text(data = apart_code_final, aes(label=단지명,vjust=-1))+
  geom_text(data = apart_code_final, aes(label=거래금액,vjust=-1))
