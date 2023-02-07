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

#Make a raster stack of input variables to make a map
#BurnSev, LOGGED, STAND_AGE, WTED_PROJ_HT, CR_CLOSURE,
#LIVE_STEMS, TOT_STEMS, PC_DEAD, 
#PC_broadleaf, SHRB_CC, NVEG_PCT, SITE_INDEX

VRIr<-rast(file.path(spatialOutDir,"VRIr.tif"))
BEC_LUT<-read_csv(file.path(SpatialDir,'v_datadeliver_tiff_bec.csv')) %>%
  
VRI_data<-read_csv(file.path(dataOutDir,'VRIr_data.csv')) %>%
  dplyr::select(VRI_id,PROJ_AGE_1,PROJ_HEIGHT_1,CROWN_CLOSURE,VRI_LIVE_STEMS_PER_HA,
                STAND_PERCENTAGE_DEAD, SHRUB_CROWN_CLOSURE,SITE_INDEX,
                PC_broadleaf,NVEG_PCT)
#VRI_data[is.na(VRI_data)] <- 0
#Function to take VRI attribute and make raster for generating stack

VRI_mapFn <- function(j) {
    VRIClassify <- VRI_data %>% dplyr::select(VRI_id, VRIatts[j])
    Modelattribute<-raster(classify(VRIr,VRIClassify))
    return(Modelattribute)
}

VRIatts<-c('PROJ_AGE_1','PROJ_HEIGHT_1','CROWN_CLOSURE','VRI_LIVE_STEMS_PER_HA',
           'STAND_PERCENTAGE_DEAD', 'SHRUB_CROWN_CLOSURE','SITE_INDEX',
           'PC_broadleaf','NVEG_PCT')

ModelAtts<-c('STAND_AGE','WTED_PROJ_HT','CR_CLOSURE','LIVE_STEMS',
            'PC_DEAD', 'SHRB_CC','SITE_INDEX',
            'PC_broadleaf','NVEG_PCT')

VRIraster<-lapply(1:length(VRIatts), function (i) VRI_mapFn(i))

VRIbrick<-brick(VRIraster)
names(VRIbrick)<-ModelAtts

writeRaster(VRIbrick, file.path(spatialOutDir,names(VRIbrick)), bylayer=TRUE, format='GTiff')


#Random Forest Severity Map
rfBrick<-brick(VRIbrick$STAND_AGE, VRIbrick$WTED_PROJ_HT, VRIbrick$CR_CLOSURE, 
               VRIbrick$PC_broadleaf, VRIbrick$PC_DEAD, VRIbrick$NVEG_PCT)

rf_map <- predict(rfBrick,rf)
writeRaster(rf_map, file.path(spatialOutDir,'rf_map'), bylayer=TRUE, format='GTiff')

#CTREE severity map
rfBrick<-brick(VRIbrick$STAND_AGE, VRIbrick$WTED_PROJ_HT, VRIbrick$CR_CLOSURE, 
               VRIbrick$PC_broadleaf, VRIbrick$PC_DEAD, VRIbrick$NVEG_PCT)

ctree_map <- predict(rfBrick,irisct,type="prob")

writeRaster(ctree_map, file.path(spatialOutDir,'ctree_map'), bylayer=TRUE, format='GTiff')

