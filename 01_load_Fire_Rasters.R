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


#Load Fire date of burn, Fire run and individual weather rasters for each fire
#Function to read in fire weather data from directory
RastReadFn <- function(FireNum){
#get the file names for a fire's weather data
Fire_files<-list.files(path=file.path(DataDir,'Fire/FireWeatherRasters'), 
                       recursive=TRUE, pattern=FiresOfInterest[FireNum])
#Read each of the rasters in using Raster
WeatherRasters<-lapply(file.path(DataDir,'Fire/FireWeatherRasters',
                                 Fire_files), raster)
#resample so they all use the Provincial Raster extent with 100m cells
ResampleRast<-ProvRast#WeatherRasters[[1]]
WeatherRastersR <-c(lapply(c(1:length(WeatherRasters)), function(j) 
  raster::resample(WeatherRasters[[j]],ResampleRast,method='ngb')))
}
#Make a list of lists of the fire's rasters
WeatherRastersR <-lapply(c(1:length(FiresOfInterest)), function(i) RastReadFn(i))
names(WeatherRastersR)<-FiresOfInterest
saveRDS(WeatherRastersR,file='tmp/WeatherRastersR')

#Get a list of the weather attributes
WeatherAttributes<-list.files(path=file.path(DataDir,'Fire/FireWeatherRasters'), 
  recursive=TRUE, pattern=FiresOfInterest[1]) %>%
  gsub("\\_.*","",.)
saveRDS(WeatherAttributes,file='tmp/WeatherAttributes')

  


