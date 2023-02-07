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
# See the License for the specific language governing permissions and limitations under the License

# make polygon coverages of each DOB and store in a list
# for raster extract in analysis routine\
#Load data
DOBRastersRr<-readRDS('tmp/DOBRastersRr')
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

#################
#use poly coverage to do raster extract of IDW weather rasters
# only for days that are not in data set?

WeatherAtts<-c('fwi','wind_direction','wind_speed','isi')
WeatherDays<-c(min((WeatherData %>% bind_rows())$wDay):max((WeatherData %>% bind_rows())$wDay))
WeatherDays<-c(116:304)
#WeatherDays<-c(116:118)
#For each weather attribute read in each day and make a raster stack
WeatherDayStacks<-lapply(c(1:length(WeatherAtts)), function(i) {
  #For each day
  WStack<-stack(lapply(c(1:length(WeatherDays)), function(j) {
     raster(file.path(spatialOutDir,paste0('IDW/',WeatherAtts[i],'_',WeatherDays[j],'.tif')))
  }))
  names(WStack)<-WeatherDays
  return(WStack)
})
names(WeatherDayStacks)<-WeatherAtts

# Extract raster value from weather attribute stacks and summarize for each DOB polygon
DOBpolyLL<-lapply(c(1:length(FiresOfInterest)), function(k) {
    Reduce(left_join, lapply(c(1:length(WeatherAtts)), function(l) {
    WeatherEx<-exact_extract(WeatherDayStacks[[l]], DOBpolyL[[k]], 'mode', 
                             force_df=TRUE, append_cols=c('Fire','poly_id'))
    colnames(WeatherEx)<-c('Fire','poly_id',paste0('Day',as.character(WeatherDays)))
    WeatherExP<-tidyr::pivot_longer(WeatherEx,cols = starts_with("Day"), names_to = "wDay",  
                                    values_to = WeatherAtts[l], names_prefix = "Day")
    return(WeatherExP)
}))
})

DOBPolyLW<-do.call(rbind.data.frame, DOBpolyLL)
saveRDS(DOBPolyLW,file='tmp/DOBPolyLW')

DOBPolyLW<-DOBPolyLW %>% dplyr::mutate(wDay=as.numeric(wDay))
  
#Merge fire specific weather data with IDW data
FireDOBdataD<-FireDOBdata %>% st_drop_geometry() #%>%
  #mutate(wDay=as.character(wDay))
Fire_area_LUT<-FireDOBdataD %>%
  dplyr::select(Fire,poly_id,area_Ha)

FireWeather<-DOBPolyLW  %>%
  dplyr::rename(IDW_fwi=fwi) %>%
  dplyr::rename(IDW_isi=isi) %>%
  left_join(FireDOBdataD, by=c('Fire','wDay','poly_id')) %>%
  mutate_at(c('area_Ha',WeatherAttributes), ~replace_na(.,0)) #%>%
  #dplyr::select(-c('area_Ha')) %>%
  #left_join(Fire_area_LUT,by=c('Fire','poly_id'))
  
saveRDS(FireWeather,file='tmp/FireWeather')



