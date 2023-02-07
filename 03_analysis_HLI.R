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

#fwi is a normal distribution
#area is negative exponential
# see below for exploritory code to determine.
# https://rpubs.com/eraldagjika/715261

elev<-rast(file.path(spatialOutDir,'LakesLDEM.tif'))
Union_Fires.v<-vect(st_read(file.path(spatialOutDir,"Union_Fires.geo.gpkg"))) 

#Calculate heat load using sptialEco package routine
heat.load <- hli(elev)
plot(heat.load, main="Heat Load Index") 


#make a unique id - for joining
Union_Fires.v$ID_UF <- 1:nrow(Union_Fires.v)
#Terra extract to get mean dNBR for each polygon
UF.HL.1<-terra::extract(heat.load, Union_Fires.v, method="simple",fun=mean,ID=TRUE)

#Add extracted values back to Union_Fires
UF.HL.v<-cbind(Union_Fires.v, UF.HL.1)

writeVector(UF.HL.v, file.path(spatialOutDir,"UF.HL.v.gpkg"),overwrite=TRUE)
