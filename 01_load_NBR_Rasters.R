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

#Read dNBR rasters, resample to project raster allignment and save
Fire_files<-lapply(c(1:length(FiresOfInterest)), function(i) 
  paste0(file.path(DataDir,'Fire/dNBR/NBRkeep/'),
  list.files(path=file.path(DataDir,'Fire/dNBR/NBRkeep'),recursive=FALSE, pattern=FiresOfInterest[i]))
  )

stk1<- terra::rast(unlist(Fire_files))
dNBR<- terra::resample(stk1,rast(ProvRast),method='ngb')
writeRaster(dNBR, filename=file.path(spatialOutDir,'dNBR.tif'), overwrite=TRUE)







#####
stk2<-raster::stack(stk1)
stk3<-raster::resample(stk2,ProvRast,method='ngb')
names(stk2)<-FiresOfInterest
writeRaster(stk3, filename=file.path(spatialOutDir,names(stk3)), bylayer=TRUE,format="GTiff")


#Function to read in unioned fire data from directory
RastReadFn <- function(FireNum){
#get the file names for a fire's weather data
Fire_files<-list.files(path=file.path(DataDir,'Fire/dNBR/NBRkeep'), 
                       recursive=TRUE, pattern=FiresOfInterest[FireNum])
#Read each of the rasters
NBRRasters<-lapply(file.path(DataDir,'Fire/dNBR/NBRkeep',
                                 Fire_files), raster)
#resample so they all use the Provincial Raster extent with 100m cells
ResampleRast<-ProvRast#DOBRasters[[1]]
NBRRastersR1 <-c(lapply(c(1:length(NBRRasters)), function(j) 
  raster::resample(NBRRasters[[j]],ResampleRast,method='ngb')))
}
#Make a list of lists of the fire's rasters
NBRRastersRr <-lapply(c(1:length(FiresOfInterest)), function(i) RastReadFn(i))

saveRDS(NBRRastersRr,file='tmp/NBRRastersRr')

