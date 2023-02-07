# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

NBRRastersRr<-readRDS('tmp/NBRRastersRr')
WeatherAttributes<-readRDS(file='tmp/WeatherAttributes')

DOBpolyFn <- function(j){
  DOBp<-rasterToPolygons(DOBRastersRr[[j]][[1]], dissolve=TRUE) %>%
    sf::st_as_sf() %>%
    st_cast("POLYGON") %>%
    mutate(Fire=FiresOfInterest[j])
  DOBp$poly_id <- 1:nrow(DOBp)
  colnames(DOBp)<-c('wDay','geometry','Fire','poly_id') 
  DOBp<-DOBp %>%
    dplyr::select(Fire,wDay,geometry,poly_id) %>%
    relocate(geometry, .after = last_col())
  return(DOBp)
}
DOBpolyL<-lapply(c(1:length(FiresOfInterest)), function(i) DOBpolyFn(i))
names(DOBpolyL)<-FiresOfInterest
saveRDS(DOBpolyL,file='tmp/DOBpolyL')
DOBpolyLcheck<-DOBpolyL[[1]]

#use poly coverage to do raster extract of DOB weather rasters
DOBAttFn <- function(j){
  FireDOBdat1<-exact_extract(stack(WeatherRastersR[[j]]), DOBpolyL[[j]], 'mode') %>%
    mutate(poly_id=as.numeric(rownames(.))) %>%
    mutate(Fire=FiresOfInterest[j])
  colnames(FireDOBdat1)<-c(WeatherAttributes,'poly_id','Fire')
  
  #add fire id for join
  FireDOBdat<-DOBpolyL[[j]] %>%
    left_join(FireDOBdat1) %>%
    mutate(area_Ha=as.numeric(st_area(.)*0.0001)) %>%
    relocate(geometry, .after = last_col())
  return(FireDOBdat)
} 

FireDOBdata1<-lapply(c(1:length(FiresOfInterest)), function(i) DOBAttFn(i))
names(FireDOBdata1)<-FiresOfInterest

tt<-FireDOBdata1[[1]]
#Rejoin with spatial

FireDOBdata<-do.call(rbind.data.frame, FireDOBdata1)
write_sf(FireDOBdata, file.path(spatialOutDir,"FireDOBdataall.gpkg"))

min(FireDOBdata$wDay)
max(FireDOBdata$wDay)





#Load Data
FireWeather<-readRDS(file='tmp/FireWeather')

#Some histograms and density plots
hist(FireWeather$area_Ha, freq = FALSE)
lines(density(FireWeather$area_Ha))

xlim=c(0,2000)
plot(FireWeather$area_Ha,FireWeather$IDW_fwi)

#Use ggplot to make nicer graphs
ggplot(data=FireWeather, aes(x=area_Ha, y=IDW_fwi)) +
  geom_point() +
  xlim(0, 20000) +
  ylim(0, 40) +
  geom_smooth(method=lm, se=FALSE)

#Explore a single fire
FireWeatherF<-FireWeather %>% dplyr::filter(Fire=='R11796')

#Group by Fire and Day, filter for time only during fires burning to reduce 0s in data
FireDataG<-FireWeather %>%
  group_by(Fire,wDay) %>%
  dplyr::summarise(n=n(), area=sum(area_Ha), wind=max(wind_speed), fwi=max(IDW_fwi),
        wind_direction=first(wind_direction)) %>%
  dplyr::filter(wDay>208 & wDay<245)

ggplot(data=FireDataG, aes(x=area, y=fwi)) +
  geom_point() +
  xlim(0,5500) +
  ylim(0, 65) +
  geom_smooth(method=lm, se=FALSE)

#quick and dirty regression model
attach(FireDataG)
model <- lm(area~fwi)
summary(model)

#Look at outliers - there are lots!
boxplot(FireDataG$area)











