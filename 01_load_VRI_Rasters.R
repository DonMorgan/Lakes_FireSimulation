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
FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")

#Function to read in fire DOB data from directory
VRIRastReadFn <- function(FireNum){
#get the file names for a fire's weather data
VRI_files<-list.files(path=file.path(DataDir,'Fire/VRIpreds'), 
                       recursive=TRUE, pattern=FiresOfInterest[FireNum])
#Read each of the rasters in using Terra
VRIRasters<-lapply(file.path(DataDir,'Fire/VRIpreds',
                             VRI_files), raster)
#resample so they all use the Provincial Raster extent with 100m cells
ResampleRast<-ProvRast#VRIRasters[[1]]
VRIRastersR1 <-c(lapply(c(1:length(VRIRasters)), function(j) 
raster::resample(WeatherRasters[[j]],ResampleRast,method='ngb')))
}
#Make a list of lists of the fire's rasters
VRIRastersR <-lapply(c(1:length(FiresOfInterest)), function(i) VRIRastReadFn(i))
saveRDS(VRIRastersR, file='tmp/VRIRastersR')

