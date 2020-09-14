load("E:/Dissertation/greater-london-latest/.RData")
install.packages("plyr","crs","r#mapshaper","rgdal","numbers","maptools","caret" )
library(plyr)
library(crs)
library(rmapshaper)
library(rgdal)
library(numbers)
library(maptools)
library(caret)
library(sf)
library(tmap)
library(openair)
library(dplyr)

getwd()
setwd('E:/Dissertation/')
tmap_mode("view")
getwd()
setwd('E:/Dissertation/ParticleMatter2.5PollutionImpact')
getwd()
OSM <- st_read("E:\Dissertationaurn_detailed <- importMeta(source = 'aurn',all=TRUE)
\greater-london-latest\gis_osm_natural_free_1.shp")
OSM <- st_read("E:/Dissertation/greater-london-latest/gis_osm_natural_free_1.shp")
typeof(OSM)
OSM <- ms_simplify(OSM,0.1)
OSM
OSM <- st_read("Wards__December_2016__Boundaries-shp/Wards__December_2016__Boundaries.shp")
OSM1 <- ms_simplify(OSM,0.5)
OSM1 <- transform(OSM,27700)
OSM1
#tm_shape(OSM)+tm_polygons(col="darklategray1")
tm_shape(OSM)+tm_polygons(col="darkslategray1")



library(openair)


head(kc1)
kc1 <- importAURN(site = "kc1", year = 2020)
head(kcl)
head(kc1)
summary(kc1)
kc1$site
unique(kc1$site)
aurn_meta <- importMeta(source = "aurn")
aurn_meta
aurn_meta <- importMeta(source = "aurn", all = TRUE)
head(aurn_meta)
aurn_pm25_sites <- filter(aurn_meta, variable == "PM2.5")
aurn_detailed <- importMeta(source="aurn",all = TRUE)
aurn_detailed
aurn_pm25_sites <- filter(aurn_detailed, variable == "PM2.5")
aurn_pm25_sites <- filter(aurn_detailed, variable == "PM2.5")
aurn_pm25_sites <- filter(aurn_detailed, 'variable' == "PM2.5")
nrows(aurn_pm25_sites)
nrow(aurn_pm25_sites)
aurn_pm25_sites
aurn_pm25_sites
aurn_detailed

unique(aurn_detailed$site)
filter(aurn_detailed,grepl('London',site))
filter(aurn_detailed$site,grepl('London',site))
typeof(aurn_pm25_sites)
names(aurn_pm25_sites)

aurn_pm25_sites.names()
aurn_detailed
pm25 <- filter(aurn_detailed,
variable == "NO2",
site_type == "Urban Traffic")
nrow(pm25)
aurn_detailed %>% filter(site == "Aberdeen")
aurn_detailed <- importMeta(source = "aurn", all = TRUE)
no2_sites <- filter(
aurn_detailed,
variable == "NO2",
site_type == "Urban Traffic"
)
nrow(no2_sites)
no2_sites <- filter (aurn_detailed,variable=="NO2")
no2_sites <- filter (aurn_detailed,variable.names()=="NO2")
no2_sites <- filter (aurn_detailed,variable == "NO2")
  aurn_detailed %>% filter('variable'=='PM2.5')
pm25
PM25 <- filter(aurn_detailed,variable=="PM2.5")
PM25
PM25_LON <- filter(PM25,grepl('London',site))
PM25_LON$site[1]


lonpm25_data <- importAURN(site = PM25_LON$site, year = 2019:2020, pollutant = c("pm2.5"))
lonpm25_data <- importAURN(site = PM25_LON$code, year = 2020, pollutant = "pm2.5", meta = TRUE)
lonpm25_data
glimpse(lonpm25_data)

 install.packages("leaflet")

 
 

 pm25_data <- importAURN(site = PM25$code, year = 2020, pollutant = "pm2.5", meta = TRUE)
 pm25_data
 unique(pm25_data$date)
 glimpse(pm25_data)
 pm25_data$date = as.Date(pm25_data$date)

df <- pm25_data %>% group_by(site,code,date)%>%summarise_each(funs(mean))
df1 <- df[c('date','pm2.5')]
df1 <- df1 %>% filter(!is.na(pm2.5))
unique(df1$pm2.5)
df2 <- df1%>%group_by(date)%>%summarise_each(funs(mean))
df2






covidTrans_dat = read.csv("COVID-19-transport-use-statistics.csv")
covidTrans_dat$ï..Date1..weekends.and.bank.holidays.in.grey.






#-------------------------------------------










library(leaflet)

aurn_unique <- distinct(lonpm25_data, site, .keep_all = TRUE)

# information for map markers
content <- paste(
  paste(
    aurn_unique$site,
    paste("Code:", aurn_unique$code),
    paste("Start:", aurn_unique$start_date),
    paste("End:", aurn_unique$end_date),
    paste("Site Type:", aurn_unique$site_type),
    sep = "<br/>"
  )
)


# plot map
leaflet(aurn_unique) %>%
  addTiles() %>%
  addMarkers(~ longitude, ~ latitude,
             #popup = content,
             clusterOptions = markerClusterOptions())

aurn_unique

typeof(lonpm25_data)

osm <- st_as_sf(x = lonpm25_data, 
         coords = c("longitude", "latitude"),
         crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(osm)
typeof(osm)
tm_shape

layers(osm)

st_layers(osm)

#install.packages("tmap")


#importAQE

aqe_meta <- importMeta(source = "aqe", all = TRUE)
unique(aqe_meta$variable)
aqe_meta

aqe_pm25_sites <- filter(aqe_meta,variable=="PM2.5")
#aqe_pm25_sites <- filter(aqe_meta,variable=="PM2.5")
aqe_pm25_sites$site

load("E:/Dissertation/ParticleMatter2.5PollutionImpact/.RData")
aqe_data = read.csv("aqEngPM25Dat.csv")
aqe_data
View(kc1)
aqe_data
plot(x=aqe_data$pm2.5,y=hour(aqe_data$date))
library(lubridate)
plot(x=aqe_data$pm2.5,y=hour(aqe_data$date))
aqe_data <- importAQE(site = aqe_pm25_sites$code ,year =  2010:2020,pollutant="PM2.5",meta=TRUE)
#aqe_data <- importAQE(site = aqe_pm25_sites$code , meta = TRUE)
#,year = 2020, pollutant = "PM2.5")
aqe_data




getwd()
install.packages("osmdata")
install.packages("curl")
install.packages("selectr")
library(sf)
library(osmdata)
library(selectr)
library(sp)
q = opq(bbox = "England, Great Britain")
q = add_osm_feature(q, key = "highway")
dat = osmdata_sf(q)
lines = dat$osm_lines
pol = dat$osm_polygons



pol = st_cast(pol, "MULTILINESTRING")
pol = st_cast(pol, "LINESTRING")
lines = rbind(lines, pol)
lines = lines[, c("osm_id", "highway")]
lines = st_transform(lines, 32636)
plot(lines)
getbb("Great Britain",)
add_osm_feature("addr:state")
head(available_features())
q <- getbb("London") %>% opq() #


OSM1 <- osmdata_sf(q)
OSM1
str(q)



load("E:/Dissertation/ParticleMatter2.5PollutionImpact/.RData")
aqe_data = read.csv("aqEngPM25Dat.csv")
head(aqe_data)
#View(kc1)/


aqe_data
plot(x=aqe_data$pm2.5,y=hour(aqe_data$date))
library(lubridate)
plot(x=aqe_data$pm2.5,y=hour(aqe_data$date))
 
 
 
plot(x=hour(aqe_data$date),y=aqe_data$pm2.5,
      pch=18,
      cex=2,
      col="#3182bd",
      xlab="Time of Day",
      ylab="PM2.5 Level",
      main="Daily PM2.5 Trend") 

aqe_df <- data.frame(aqe_data)
typeof(aqe_df)
nullvals <- aqe_df %>% filter(is.na(latitude))
unique(nullvals$code)
nullvals2 <- aqe_df %>% filter(is.na(longitude))
nullvals2
unique(nullvals2$code)

getdf <- aqe_df %>% filter(!is.na(latitude))
unique(getdf$code)

osm3 <- st_as_sf(getdf,coords = c("latitude","longitude"), crs = 27700)
osm3
tm_shape(osm3)+tm_dots()
#-------------------------------------------------------------------
  
OSM2 <- st_read("Local_Authority_Districts__December_2009__Boundaries-shp/Local_Authority_Districts__December_2009__Boundaries.shp")
#OSM2 <- ms_simplify(OSM2,0.5)
OSM2 <- transform(OSM2,27700)
OSM2

tmap_mode("view")
#tm_shape(OSM)+tm_polygons(col="darklategray1")
tm_shape(OSM2)+tm_polygons(col="darkslategray1")

type(osm2)



aqe_data = read.csv("aqEngPM25Dat.csv")
aqe_df <- data.frame(aqe_data)
getdf <- aqe_df %>% filter(!is.na(latitude))
getdf
unique(getdf$code)

#getdf1 <- getdf
#coordinates(getdf1)= ~latitude+longitude
#proj4string(getdf1)=CRS("+init=epsg:4326") # set it to lat-long
#getdf1 = spTransform(getdf1,CRS(27700))


osm3 <- st_as_sf(getdf,coords = c("longitude","latitude"), crs = st_crs(OSM2))
osm3
osm4<-filter(osm3,hour(osm3$date)==09)
osm4<-filter(osm4,year(osm4$date)==2019)
osm4<-filter(osm4,month(osm4$date)==1)
osm4<-filter(osm4,day(osm4$date)==1)
osm4 <- osm4 %>% filter(!is.na(pm2.5))
osm4 <- st_transform(osm4,st_crs(OSM2))
unique(osm4$geometry)
unique(osm4$site)
osm4
OSM2

tm_shape(OSM2)+tm_polygons(col="darkslategray1")+
tm_shape(osm4)+tm_dots("pm2.5",size=0.5,alpha=0.8) #0.01)

unique(osm4$geometry)
unique(osm4$code)



ldnOsm <- st_read("London_Borough_Excluding_MHW.shp")
#OSM2 <- ms_simplify(OSM2,0.5)
ldnOsm <- st_transform(ldnOsm,27700)
ldnOsm
tm_shape(ldnOsm)+tm_polygons(col="darkslategray1")+ 
tm_shape(osm4)+tm_dots("pm2.5",size=0.5,alpha=0.8) #0.01)


------------------------------------------------------------------------
#  importKCL
aqe_meta <- importMeta(source = "aqe", all = TRUE)

unique(aqe_meta$variable)
aqe_meta
aqe_pm25_sites <- filter(aqe_meta) 
#,variable=="PM2.5")
#aqe_pm25_sites <- filter(aqe_meta,variable=="PM2.5")
unique(aqe_pm25_sites$code)
unique(aqe_pm25_sites$site)

lonpm25_data <- importKCL(year = 2015:2020,meta=TRUE)
lonpm25_data <- importKCL(site = aqe_pm25_sites$code , year = 2015:2020) 
#lonpm25_data.columns()
lonpm25_data1$site
unique(lonpm25_data$site)
typeof(lonpm25_data)

ldn3 <- st_as_sf(lonpm25_data,coords = c("longitude","latitude"), crs = 27700)
unique(ldn3$geometry)
ldn4<-filter(ldn3,hour(ldn3$date)==09)
ldn4<-filter(ldn4,year(ldn4$date)==2019)
#ldn4<-filter(ldn4,month(ldn4$date)==1)
#ldn4<-filter(ldn4,day(ldn4$date)==1)
#ldn4 <- ldn4 %>% filter(!is.na(pm2.5))
ldn4 <- st_transform(ldn4,st_crs(ldnOsm))
ldn4
tm_shape(ldnOsm)+tm_polygons(col="darkslategray1")+ 
tm_shape(ldn4)+tm_dots("pm10",size=0.5,alpha=0.8) 


#------------------------------------------------
library(plyr)
library(crs)
library(rmapshaper)
library(rgdal)
library(numbers)
library(maptools)
library(caret)
library(sf)
library(openair)
library(leaflet)
library(tmap)
library(dplyr)
library(readxl)
library(lubridate)


getwd()
OSM2 <- st_read("statistical-gis-boundaries-london/ESRI/MSOA_2004_London_High_Resolution.shp")
OSM2 <- st_transform(OSM2,crs=27700)
OSM2
tmap_mode('view')
tm_shape(OSM2)+tm_polygons(col="#d1e5f0")
+ 
  tm_shape(ldn4)+tm_dots("pm10",size=0.5,alpha=0.8) 

df1 <- data.frame(read_excel('underlying_data_2020_06_01.xlsx',sheet='1 deaths'))
df2 <- data.frame(read_excel('underlying_data_2020_06_01.xlsx',sheet='2 population'))
df3 <- data.frame(read_excel('underlying_data_2020_06_01.xlsx',sheet='7 medical_conditions'))
excel_sheets('underlying_data_2020_06_01.xlsx')
typeof(df1)
df1
df2

df4 <- merge(merge(df1,df2,all=TRUE,by='MSOA11CD'),df3,all=TRUE,by='MSOA11CD')
names(df1)
names(df2)
names(df3)
names(df4)

OSM2
OSM3 <- merge(OSM2,df4,all=TRUE,by.x='MSOA_CODE',by.y='MSOA11CD')
OSM3 <- st_transform(OSM3,27700)

tm_shape(OSM3)+tm_polygons(
  #col="#d1e5f0")
"total_registered_patients", 
#style=c("jenks", "pretty"),
palette="YlGnBu",
style="jenks",
midpoint=FALSE,
border.lwd =0,
border.alpha=0.1)

OSM3


aqe_data = read.csv("aqEngPM25Dat.csv")
aqe_df <- data.frame(aqe_data)
unique(aqe_df$code)
aqe_dat <- aqe_df %>% filter(!is.na(latitude)) %>% filter(!is.na(longitude))
osm_pm <- st_as_sf(aqe_dat,coords = c("longitude","latitude"), crs = st_crs(OSM3))
unique(osm_pm$geometry)

osm_pm1<-filter(osm_pm,hour(osm_pm$date)==09)
osm_pm1<-filter(osm_pm1,year(osm_pm1$date)==2019)
osm_pm1<-filter(osm_pm1,month(osm_pm1$date)==1)
osm_pm1<-filter(osm_pm1,day(osm_pm1$date)==1)
osm_pm1 <- osm_pm1 %>% filter(!is.na(pm2.5))
osm_pm1 <- st_transform(osm_pm1,st_crs(OSM3))

osm_pm1
osm4
OSM3

tmap_mode('view')
tm_shape(OSM3)+tm_polygons(
  #col="#d1e5f0")
  "total_registered_patients", 
  #style=c("jenks", "pretty"),
  palette="YlGnBu",
  style="jenks",
  midpoint=FALSE,
  border.lwd =0,
  border.alpha=0.1) + 
  tm_shape(osm4) + tm_dots("pm2.5",size=0.1,alpha=0.8)


#+
#  tm_legend(show=FALSE)+
#  tm_layout(frame=FALSE)+
#  tm_borders(lwd=1) +
#  tm_credits("2006", position=c(0,0.85), size=1)

#------

tfdat <- read.csv('C:/CASA_SDAV/Dissertation/Data/dft_traffic_counts_raw_counts/dft_traffic_counts_raw_counts.csv')
tfdf <- data.frame(tfdat)
head(tfdat %>% filter(region_name == 'London'))
lntf <- tfdat %>% filter(region_name == 'London')
unique(tfdat$year)
summarise(lntf)

lntf %>% filter(year == 2020)

colnames(lntf)

lnosm <- st_as_sf(lntf,coords = c("longitude","latitude"), crs = st_crs(OSM3))
unique(lnosm$year)
lnosm1 <- lnosm %>%filter(year==2018)
tm_shape(lnosm1,year=2018)+tm_dots("all_motor_vehicles",size=0.1,alpha=0.8)

unique(lnosm1$local_authoirty_ons_code)
getwd()
osm4 <- st_read("./statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")

tst <- osm4 %>% filter(count_date=='2018-03-19')
tst <- tst %>% filter(hour == 16)
unique(tst$NAME)

osm4 <- merge(osm4,lntf,all=TRUE,by.x='GSS_CODE',by.y='local_authoirty_ons_code')
osm4 <- osm4 %>% filter(year=='2018')
osm4 <- osm4 %>% filter(hour==16)


tm_shape(osm4)+tm_polygons(#col="#d1e5f0")
  "all_motor_vehicles", 
  #style=c("jenks", "pretty"),
  palette="YlGnBu",
  style="jenks",
  midpoint=FALSE,
  border.lwd =0,
  border.alpha=0.1)
#----------------------------------------

