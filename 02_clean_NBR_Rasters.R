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

# make polygon coverages of each NBR and store in a list
# for raster extract in analysis routine
#Load data into terra rast and vect formats
dNBR <- rast(file.path(spatialOutDir,'dNBR.tif'))
Union_Fires.v<-vect(st_read(file.path(spatialOutDir,"Union_Fires.geo.gpkg"))) 

#make a unique id - for joining
Union_Fires.v$ID_UF <- 1:nrow(Union_Fires.v)
Union_Fires.v.data<-data.frame(Union_Fires.v)
#Terra extract to get mean dNBR for each polygon
UF.1.mean<-terra::extract(dNBR, Union_Fires.v, method="simple",fun=mean,ID=TRUE)
UF.1.max<-terra::extract(dNBR, Union_Fires.v, method="simple",fun=max,ID=TRUE)

dNBR.names<-c('dNBR_R11796','dNBR_R11498','dNBR_R21721','dNBR_R11921','dNBR_G41607','dNBR_G51632')
#Add extracted values back to Union_Fires
UF.NBR.mean<-UF.1.mean %>%
  rename_at(vars(dNBR.names),function(x) paste0(x,".mean")) %>%
  dplyr::rename(ID_UF=ID) %>%
  rowwise() %>%
  mutate(dNBR.mean=max(dNBR_R11796.mean,dNBR_R11498.mean,dNBR_R21721.mean,dNBR_R11921.mean,dNBR_G41607.mean,dNBR_G51632.mean, na.rm=TRUE)) %>%
  ungroup() 

UF.NBR.max<-UF.1.max %>%
  rename_at(vars(dNBR.names),function(x) paste0(x,".max")) %>%
  dplyr::rename(ID_UF=ID) %>%
  rowwise() %>%
  mutate(dNBR.max=max(dNBR_R11796.max,dNBR_R11498.max,dNBR_R21721.max,dNBR_R11921.max,dNBR_G41607.max,dNBR_G51632.max, na.rm=TRUE)) %>%
  ungroup()

UF.NBR.data<-Union_Fires.v.data %>%
  left_join(UF.NBR.mean, by="ID_UF") %>%
  left_join(UF.NBR.max, by="ID_UF")

write_csv(UF.NBR.data,file.path(dataOutDir,paste0('UF.NBR.data.data.csv')))

