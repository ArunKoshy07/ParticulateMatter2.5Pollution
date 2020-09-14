library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(openair)
library(stringr)
library(corrr)
library(corrplot)
library(EnvStats)
library(PerformanceAnalytics)
library(readxl)
library(janitor)
library(spgwr)

OSM2 <- st_read("statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")
OSM3 <- st_read('statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp')
OSM2 <- st_transform(OSM2,crs=27700)
OSM2
tmap_mode('view')
tm_shape(OSM2)+tm_polygons()
colnames(OSM2)

OSM3 <- st_transform(OSM3,crs=27700)
unique(OSM3$NAME)
tmap_mode('view')
tm_shape(OSM3)+tm_polygons()
colnames(OSM3)
OSM3

em_dat <- read.csv('shpdat/LAEI2016_Emissions_Summary-NOxPMCO2_v1/2016_EmissioNS_PM215_T1.csv')
colnames(em_dat)[46] <- 'LAEI_Grand_total'
colnames(em_dat)
unique(em_dat$Source)
temp <- left_join(OSM3,em_dat,by= c("NAME" = "Source"))
colnames(temp)[52] <- 'LAEI_Grand_total'
temp$NAME
temp
temp1 <- temp[c("NAME","GSS_CODE","HECTARES","NONLD_AREA","ONS_INNER")]
colnames(temp$NAME)
unique(temp$Transport)
temp[['transden']] <- ((temp[['Transport']] * 1000)/ temp[['HECTARES']])
temp[['Domden']] <- ((temp[['Domestic']] * 1000)/ temp[['HECTARES']])
temp[['LAEIGTotden']] <- ((temp[['LAEI_Grand_total']] * 1000)/ temp[['HECTARES']])

temp$GSS_CODE

tm_shape(temp)+tm_polygons(col = "transden", 
                           palette = "YlOrBr")
tm_shape(temp)+tm_polygons(col = "Domden", 
                           palette = "YlOrBr")
tm_shape(temp)+tm_polygons(col = "LAEIGTotden", 
                           palette = "YlOrBr")
colnames(OSM2)


lsoa_dat <- read.csv("lsoaDat.csv")
colnames(lsoa_dat)[1] <- 'Codes'
lsoa_dat  

OSM21 <- st_read("statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")
OSM21 <- st_transform(OSM21,crs=27700)
unique(OSM21$LAD11CD)
OSM2 <- left_join(OSM21,lsoa_dat,by= c("LSOA11CD" = "Codes"))
OSM2 %>%filter(MSOA11CD=='E02000001' )

x2<-left_join(OSM2,st_drop_geometry(temp),by= c("LAD11CD" = "GSS_CODE"))
x2

tmap_mode('view')
tm_shape(OSM2)+tm_polygons()
temp <- st_transform(temp,crs=27700)
x1<-st_join(OSM2,temp,left=TRUE)
x1%>%filter(MSOA11CD=='E02000001' )
x2
x2[['TransEm']] <- ((x2[['transden']] * x2[['Hectares']])/1000)
x2[['DomEm']] <- ((x2[['Domden']] * x2[['Hectares']])/1000)
x2[['TotEm']] <- ((x2[['LAEIGTotden']] * x2[['Hectares']])/1000)

x12 <- x2 %>% filter(TotEm <= 1.25 )
fig <- plot_ly(type = 'box')
fig <- fig %>% add_boxplot(y = x12$TotEm, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
                           marker = list(color = 'rgb(7,40,89)'),
                           line = list(color = 'rgb(7,40,89)'),
                           name = "Total Emission")
fig

x12 <- x2 %>% filter(TransEm <= 0.47)
x12
tm_shape(x2)+
  tm_polygons(col = 'TransEm' ,
                         breaks = c(0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,1,2,4,6,10),
                         border.alpha=0,
                         title = "Transport Emissions in tonnes per year")
  
tm_shape(x2)+
  tm_polygons(col = 'TotEm' ,
              breaks = c(0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,1,2,4,6,10),
              border.alpha=0,
              title = "LAEI Total Emissions tonnes per year") 

tm_shape(x2)+
  tm_polygons(col = 'DomEm' ,
              breaks = c(0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,1,2,4,6,10),
              border.alpha=0,
              legend.title = "Domestic Emmisions in tonnes per year") 

colnames(x2[c('LSOA11CD',"LSOA11NM","Hectares","TransEm","DomEm","TotEm")])
x3 <- x2[c('LSOA11CD',"LSOA11NM","Hectares","TransEm","DomEm","TotEm")]%>%st_drop_geometry()
x3
write.csv(x3,file='LSOAEmissionfile.csv')
xc3 <- x2[c('MSOA11CD','MSOA11NM',"Hectares","TransEm","DomEm","TotEm")]%>%st_drop_geometry()%>%
       group_by(MSOA11CD,MSOA11NM)%>%summarise_all(sum)
xc3

write.csv(xc3,file='MSOAEmissionfile.csv')

#========================== Transform 05/09
symbox(~TransEm, 
       x2, 
       na.rm=T,
       powers=seq(-1,1,by=.1))

symbox(~TotEm, 
       x2, 
       na.rm=T,
       powers=seq(-1,1,by=.1))



ggplot(x2, aes(x=TransEm)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.05) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(x2, aes(x=TransEm^-0.3)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.05) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)


ggplot(x2, aes(x=TotEm)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.05) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(x2, aes(x=TotEm^-0.3)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.05) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

qplot(x = (TransEm), 
      y = TotEm,
      data=x2)

qplot(x = (TransEm)^-0.3, 
      y = (TotEm)^-0.3,
      data=x2)

symbox(~DomEm, 
       x2, 
       na.rm=T,
       powers=seq(-1,1,by=.1))
log(x2$DomEm)
ggplot(x2, aes(x=log(DomEm))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.05) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#============ GWR
#x2 <- x2 %>% filter(TransEm <= 0.447) %>% filter(TotEm <= 1.31 )
x21 <- x2
x21$TransEm
x21$TransEm <- x21$TransEm^-0.3
x21$TotEm <- x21$TotEm^-0.3
x21

coordsWSP <- x21 %>%
  st_centroid()%>%
  st_geometry()%>%
  as(., "Spatial")
st_crs(coordsWSP) = 27700
plot(coordsWSP)

st_crs(x21) = 27700

x1sp<- x21 %>%
  as(., "Spatial")
names(x1sp)

GWRbandwidth1 <- gwr.sel(TotEm ~ TransEm , 
                        data = x1sp, 
                        coords=coordsWSP,
                        adapt=T)

gwr.model1 = gwr(TotEm ~ TransEm , 
                data = x1sp, 
                coords=coordsWSP, 
                adapt=GWRbandwidth1, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model1

results <- as.data.frame(gwr.model1$SDF)
names(results)

x1spN <- x21 %>%
  mutate(coef_TransEm = results$TransEm)

tm_shape(x1spN) +
  tm_polygons(col = "coef_TransEm", 
              palette = "-RdYlBu", 
              breaks = c(0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2),
              alpha = 0.8,border.alpha=0.2,
              title = "GWR Coefficent for Transport Emissions")

names(gwr.model1$SDF)
#run the significance test
#sigTest = abs(gwr.model1$SDF$"TransEm") 

sigTest = abs(gwr.model1$SDF$"TransEm")-2 * gwr.model1$SDF$"TransEm_se"


#store significance results
x1spN1 <- x1spN %>%
  mutate(GWRUnauthSig = sigTest)

tm_shape(x1spN1) +
  tm_polygons(col = "GWRUnauthSig", 
#              palette = "RdYlBu",OrRd
              palette = "-RdYlBu",
              breaks = c(0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2),
              border.alpha = 0,
              title ='GWR significance')
#==================== linear model 
#x3 <- x2 %>% filter(TransEm <= 6) %>% filter(TotEm <= 10 )
x3<- x21


x3[c(4712,2615,2778),]

model_final <- lm( TotEm ~ TransEm , 
                   data = x3)

tidy(model_final)
plot(model_final)
summary(model_final)
#length(model_final$residuals)
#LonMsoaProfiles1 %>% 
#nrow(LonMsoaProfiles %>% filter(is.na(pm252013me)))
residualPlot(model_final)
x31 <- x3 %>%  mutate(model_final_res = residuals(model_final))
x31$model_final
x31
tm_shape(x31) +
  tm_polygons(col = "model_final_res", 
            #  palette = "RdYlBu",
              palette = "-RdYlBu",
              border.alpha = 0.1,
              title ='Model Residuals')


#save the residuals into your dataframe
model_data <- model_final %>%
  augment(., x3)

#plot residuals
model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 

#qtm(x31, fill = "model_final_res")
#====
coordsW <- x31 %>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()



plot(LWard_knn, st_geometry(coordsW), col="blue")

plot(x31)

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")


final_model_Moran <- x31 %>%
  st_drop_geometry()%>%
  dplyr::select(model_final_res)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

final_model_Moran
  