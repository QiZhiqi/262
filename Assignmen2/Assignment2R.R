library(tidyverse)
library(tidyr)

CA<-read_delim("C:/Users/qzq/Desktop/ESM262/Assignment/Assignment2/CA_Features_20170401.txt",delim="|")

#Import and Tide California gazetter dataset
CA1<-subset(CA,select = -c(STATE_NUMERIC,COUNTY_NUMERIC,PRIMARY_LAT_DMS,PRIM_LONG_DMS,SOURCE_LAT_DMS,SOURCE_LONG_DMS,ELEV_IN_FT))

colnames(CA1)[colnames(CA1)=="FEATURE_ID"]<-"ID"
colnames(CA1)[colnames(CA1)=="FEATURE_NAME"]<-"Name"
colnames(CA1)[colnames(CA1)=="FEATURE_CLASS"]<-"Class"
colnames(CA1)[colnames(CA1)=="STATE_ALPHA"]<-"State"
colnames(CA1)[colnames(CA1)=="COUNTY_NAME"]<-"County"
colnames(CA1)[colnames(CA1)=="FEATURE_CLASS"]<-"Class"
colnames(CA1)[colnames(CA1)=="PRIM_LAT_DEC"]<-"Latitude"
colnames(CA1)[colnames(CA1)=="PRIM_LONG_DEC"]<-"Longitude"
colnames(CA1)[colnames(CA1)=="SOURCE_LAT_DEC"]<-"SourceLatitude"
colnames(CA1)[colnames(CA1)=="SOURCE_LONG_DEC"]<-"SourceLongitude"
colnames(CA1)[colnames(CA1)=="ELEV_IN_M"]<-"Elevation"
colnames(CA1)[colnames(CA1)=="MAP_NAME"]<-"MapName"
colnames(CA1)[colnames(CA1)=="DATE_CREATED"]<-"DateCreate"
colnames(CA1)[colnames(CA1)=="DATE_EDITED"]<-"DateEdit"

CA2<-subset(CA1,CA1$State=="CA")
CA3<-subset(CA2,CA2$Latitude>0)
California<-subset(CA3,CA3$Longitude<0)
write.table(California,"California.csv",sep = "|")

#The most-frequently-occuring feature name
maxname<- group_by(California,Name)
maxname1<-summarise(maxname,count=n())
max=which.max(maxname1$count)
maxname1[max,c("Name","count")]

#The least-frequently-occuring feature class
leastclass<- group_by(California,Class)
leastclass1<-summarise(leastclass,count=n())
min=which.min(leastclass1$count)
leastclass1[min,c("Class","count")]



#The approximate center point of each county
Center<-group_by(California, County)
CenterLat<-summarise(Center,MeanLat=mean(Latitude,na.rm=TRUE))
CenterLong<-summarise(Center,MeanLong=mean(Longitude,na.rm=TRUE))
Centerpoint<-left_join(CenterLat,CenterLong,by="County")



#The fraction of the total number features in each county by manmade and nature
Class<-c("Airport","Arch","Area","Arroyo","Bar","Basin","Bay","Beach","Bench","Bend","Bridge","Building","Canal","Cape","Cave","Cemetery","Census","Channel","Church","Civil","Cliff","Crater","Crossing","Dam","Falls","Flat","Forest","Gap","Glacier","Gut","Habor","Hospital","Island","Isthmus","Lake","Lava","Levee","Locale","Military","Mine","Oifield","Park","Pilar","Plain","PopulatedPlace","PostOffice","Range","Rapids","Reserve","Reservoir","Ridge","School","Sea","Slope","Spring","Stream","Summit","Swamp","Tower","Trail","Tunnel","Unknown","Valley","Well","Woods")

#Manmade=m,Natural=n
Definition<-c("m","n","n","n","n","n","n","n","n","n","m","m","m","n","n","m","m","n","m","m","n","n","m","m","m","n","n","n","n","n","m","m","n","n","n","n","m","m","m","n","n","m","m","n","m","m","n","n","m","m","n","m","n","n","n","n","n","n","m","m","m","NA","n","m","n")
Defi<-data.frame(Class,Definition)
View(Defi)

Defi1<-select(California,c(County,Class))

Defi2<-inner_join(Defi1,Defi,by="Class")
Nature<-subset(Defi2,Defi2$Definition=="n")
Nature1<-group_by(Nature,County)
Nature2<-summarise(Nature1,count=n())

Manmade<-subset(Defi2,Defi2$Definition=="m")
Manmade1<-group_by(Manmade,County)
Manmade2<-summarise(Manmade1,count=n())
Fraction<-left_join(Nature2,Manmade2,by="County")
colnames(Fraction)[2]<-"Natural"
colnames(Fraction)[3]<-"Man-made"
Fraction$FractionNatural<-Fraction$Natural/(Fraction$Natural+Fraction$`Man-made`)
Fraction$FractionManmade<-Fraction$`Man-made`/(Fraction$Natural+Fraction$`Man-made`)







