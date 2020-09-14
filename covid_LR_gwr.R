#library a bunch of packages we may (or may not) use - install them first if not installed already. 
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

memory.limit(size=15000)

tmap_mode("view")
getwd()
cvddat <- read.csv('covidDat.csv')
em_dat <- read.csv('MSOAEmissionfile.csv')
em_dat
unique(cvddat$MSOA11CD)
unique(em_dat$MSOA11CD)

temp2 <- left_join(em_dat,cvddat,by= c("MSOA11CD" = "MSOA11CD"))
temp2

symbox(~TransEm, temp2, na.rm=T,powers=seq(-1,1,by=.1))
symbox(~TotEm, temp2, na.rm=T, powers=seq(-1,1,by=.1))
symbox(~DomEm, temp2, na.rm=T, powers=seq(-1,1,by=.1))
symbox(~covid_19_deaths,temp2,na.rm=T,powers=seq(-1,1,by=.1))


ggplot(temp2, aes(x=TransEm)) + geom_histogram(aes(y = ..density..),binwidth = 0.05) + 
  geom_density(colour="red", size=1, adjust=1)

ggplot(temp2, aes(x=TransEm^-0.6)) + geom_histogram(aes(y = ..density..), binwidth = 0.05) + 
  geom_density(colour="red", size=1,adjust=1)


ggplot(temp2, aes(x=TotEm)) + geom_histogram(aes(y = ..density..), binwidth = 0.05) + geom_density(colour="red", 
                                                                                                   size=1, adjust=1)

ggplot(temp2, aes(x=TotEm^-0.6)) +   geom_histogram(aes(y = ..density..), binwidth = 0.05) + 
  geom_density(colour="red", size=1, adjust=1)

ggplot(temp2, aes(x=DomEm)) + geom_histogram(aes(y = ..density..), binwidth = 0.05) + geom_density(colour="red", 
                                                                                                   size=1, adjust=1)
ggplot(temp2, aes(x=DomEm^-0.1)) +   geom_histogram(aes(y = ..density..), binwidth = 0.05) + 
  geom_density(colour="red", size=1, adjust=1)

qplot(x = (TransEm),  y = TotEm,      data=temp2)
qplot(x = (TransEm)^-0.6,  y = (TotEm)^-0.6,  data=temp2)



symbox(~covid_19_deaths,temp2,na.rm=T,powers=seq(-4,4,by=0.25)) #log
symbox(~covid_19_deaths_per_thousand,temp2,na.rm=T,powers=seq(-4,4,by=0.25)) #log
symbox(~total_population_mid_2018,temp2,na.rm=T,powers=seq(-4,4,by=0.5)) #-1
symbox(~over_70_prop,temp2,na.rm=T,powers=seq(-4,4,by=0.25)) #0.25
symbox(~total_registered_patients,temp2,na.rm=T,powers=seq(-4,4,by=0.25)) #-0.25
symbox(~Hypertension,temp2,na.rm=T,powers=seq(-4,4,by=0.125)) #1
symbox(~Obesity..18..,temp2,na.rm=T,powers=seq(-4,4,by=0.25)) #0.25
symbox(~Diabetes,temp2,na.rm=T,powers=seq(-4,4,by=0.25)) #0.25
symbox(~Asthma,temp2,na.rm=T,powers=seq(-4,4,by=0.25)) #1.25
symbox(~Coronary.heart.disease,temp2,na.rm=T,powers=seq(-4,4,by=0.25)) #0.5
symbox(~all_bame_prop,temp2,na.rm=T,powers=seq(-4,4,by=0.25)) #0.5


temp2$TransEm <- temp2$TransEm^-0.6
temp2$TotEm <- temp2$TotEm^-0.6
temp2$DomEm <- temp2$DomEm^-0.1
temp2$covid_19_deaths <- temp2$covid_19_deaths^0.25
temp2$covid_19_deaths_per_thousand <- temp2$covid_19_deaths_per_thousand^0.25
temp2$total_population_mid_2018 <- temp2$total_population_mid_2018^-1
temp2$over_70_prop <- temp2$over_70_prop^0.25
temp2$total_registered_patients <- temp2$total_registered_patients^-0.25
temp2$Hypertension <- temp2$Hypertension
temp2$Obesity..18.. <- temp2$Obesity..18..^0.25
temp2$Diabetes <- temp2$Diabetes^0.25
temp2$Asthma <- temp2$Asthma^1.25
temp2$Coronary.heart.disease <- temp2$Coronary.heart.disease^0.5
temp2$all_bame_prop <- temp2$all_bame_prop^0.5

#
#ggplot(temp2, aes(x=covid_19_deaths)) + geom_histogram(aes(y = ..density..),binwidth = 1) + 
#  geom_density(colour="red", size=1, adjust=1)

#ggplot(temp2, aes(x=log(covid_19_deaths))) + geom_histogram(aes(y = ..density..), binwidth = 0.25) + 
#  geom_density(colour="red", size=1,adjust=1)


#symbox(~Asthma,temp2,na.rm=T,powers=seq(-4,4,by=0.5))

#ggplot(temp2, aes(x=Asthma)) + geom_histogram(aes(y = ..density..),binwidth = 1) + 
#  geom_density(colour="red", size=1, adjust=1)

#ggplot(temp2, aes(x=log(covid_19_deaths))) + geom_histogram(aes(y = ..density..), binwidth = 0.25) + 
#  geom_density(colour="red", size=1,adjust=1)



str(temp2)
#temp2 <- temp2[,!is.na(temp2)]
unique(temp2$MSOA11CD)


OSM2 <- st_read("statistical-gis-boundaries-london/ESRI/MSOA_2004_London_High_Resolution.shp")
OSM2 <- st_transform(OSM2,crs=27700)
OSM2$MSOA_CODE
tmap_mode('view')

#OSM3 <- merge(OSM2,temp2,x.by='MSOA_CODE',y.by='MSOA11CD',all=TRUE)

LonMsoaProfiles <- OSM2 %>%
  left_join(., 
            temp2, 
            by = c("MSOA_CODE" = "MSOA11CD"))%>%
  clean_names()
LonMsoaProfiles
LonMsoaProfiles <- LonMsoaProfiles%>%filter(!is.na(covid_19_deaths)) #
str(LonMsoaProfiles)

myvars <- LonMsoaProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(trans_em,dom_em,tot_em,covid_19_deaths,covid_19_deaths_per_thousand,total_population_mid_2018,
                over_70_prop,total_registered_patients,hypertension,obesity_18,diabetes,
                asthma,coronary_heart_disease,all_bame_prop)
getwd()
write.csv(myvars,file='coviddat1.csv')
#write.csv(myvars,file='coviddat2.csv') #without transformation


model_final <- lm( covid_19_deaths ~ tot_em + 
                     obesity_18 + 
                     coronary_heart_disease, 
                   data = myvars)

tidy(model_final)
summary(model_final)
plot(model_final)
residualPlot(model_final)
LonMsoaProfiles1 <- LonMsoaProfiles %>%  mutate(model_final_res = residuals(model_final))
LonMsoaProfiles1$model_final
LonMsoaProfiles1
qtm(LonMsoaProfiles1, fill = "model_final_res")

#====
coordsW <- LonMsoaProfiles1 %>%
  st_centroid()%>%
  st_geometry()

plot(coordsW)
knn_wards <-coordsW %>%
  knearneigh(., k=4)

LWard_knn <- knn_wards %>%
  knn2nb()



plot(LWard_knn, st_geometry(coordsW), col="blue")

plot(LonMsoaProfiles1)

Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")


final_model_Moran <- LonMsoaProfiles1 %>%
  st_drop_geometry()%>%
  dplyr::select(model_final_res)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()

final_model_Moran

###### gwr

st_crs(LonMsoaProfiles1) = 27700

LonMsoaProfiles1SP <- LonMsoaProfiles %>%
  as(., "Spatial")
st_crs(coordsW) = 27700
coordsWSP2 <- coordsW %>%
  as(., "Spatial")
coordsWSP2

GWRbandwidth2 <- gwr.sel(covid_19_deaths ~ tot_em + 
                          obesity_18 + 
                          coronary_heart_disease, 
                        data = LonMsoaProfiles1SP, 
                        coords=coordsWSP2,
                        adapt=T)

gwr.model2 = gwr(covid_19_deaths ~ tot_em + 
                  obesity_18 + 
                  coronary_heart_disease, 
                data = LonMsoaProfiles1SP, 
                coords=coordsWSP2, 
                adapt=GWRbandwidth2, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model2

results <- as.data.frame(gwr.model$SDF)
names(results)

LonMsoaProfiles2 <- LonMsoaProfiles %>%
  mutate(coef_obesity_18 = results$obesity_18,
         coef_coronary_heart_disease = results$coronary_heart_disease,
         coef_tot_em = results$tot_em)

tm_shape(LonMsoaProfiles2) +
  tm_polygons(col = "coef_obesity_18", 
              palette = "OrRd", 
              alpha = 0.5)

tm_shape(LonMsoaProfiles2) +
  tm_polygons(col = "coef_coronary_heart_disease", 
              palette = "OrRd", 
              alpha = 0.5)

tm_shape(LonMsoaProfiles2) +
  tm_polygons(col = "coef_tot_em", 
              palette = "OrRd", 
              alpha = 0.5)


names(gwr.model$SDF)
#run the significance test
sigTest = abs(gwr.model$SDF$"tot_em") -2 * gwr.model$SDF$"tot_em_se"


#store significance results
LonMsoaProfiles2s <- LonMsoaProfiles2 %>%
  mutate(GWRUnauthSig = sigTest)

tm_shape(LonMsoaProfiles2s) +
  tm_polygons(col = "GWRUnauthSig", 
              palette = "OrRd",
              title ='GWR significance')

(0.675983)^(-1/0.3)
