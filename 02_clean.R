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

Clean_F<-file.path(spatialOutDir,"Lakes_TSAr.tif")
if (!file.exists(Clean_F)) {
  Lakes_TSAr<-st_read(file.path(spatialOutDir,"Lakes_TSA.gpkg")) %>%
  mutate(raster_value=1) %>%
  fasterize(ProvRast,field='raster_value') %>%
  terra::crop(Lakes_TSA) %>%
  terra::mask(Lakes_TSA)
writeRaster(Lakes_TSAr,file.path(spatialOutDir,"Lakes_TSAr.tif"),overwrite=TRUE)
Lakes_TSAr<-raster(file.path(spatialOutDir,"Lakes_TSAr.tif"))

Lakes_TSAbuf<-Lakes_TSA %>%
  st_buffer(dist=100000)
#mapview(Lakes_TSAbuf) + mapview(Lakes_TSA)
write_sf(Lakes_TSAbuf, file.path(spatialOutDir,"Lakes_TSAbuf.gpkg"))

Lakes_TSAbufr<-Lakes_TSAbuf %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(raster_value=1) %>%
  fasterize(ProvRast,field='raster_value') %>%
  terra::crop(Lakes_TSAbuf) %>%
  terra::mask(Lakes_TSAbuf)
writeRaster(Lakes_TSAbufr,file.path(spatialOutDir,"Lakes_TSAbufr.tif"),overwrite=TRUE)

#Make lager clipping polygon - using weather raster as size
#Lakes TSA within larger raster bounds
Lakes_r <-extend(Lakes_TSAr, Lakes_large)
writeRaster(Lakes_r,file.path(spatialOutDir,"Lakes_r.tif"),overwrite=TRUE)

#Make BEC raster with values assigned according to LUT
BEC_LUT<-read_csv(file.path(SpatialDir,'v_datadeliver_tiff_bec.csv')) %>%
  dplyr::rename(BGC_LABEL = becsubzone)
BECr_lks<-BEC_Lks %>%
  st_cast("MULTIPOLYGON") %>%
  fasterize(ProvRast,field='raster_value') 
writeRaster(BECr_lks,file.path(spatialOutDir,"BECr_lks.tif"),overwrite=TRUE)

#BEC_Lksr <- BEC_r %>%
#  terra::crop(AOI) %>%
#  terra::mask(AOI)
#writeRaster(BEC_Lksr,file.path(spatialOutDir,"BEC_Lksr.tif"),overwrite=TRUE)

BECr_data<-BEC_Lks %>%
  st_drop_geometry()
write.csv(BECr_data, file=file.path(dataOutDir,'BECr_data.csv'), row.names = FALSE)

BroadlSp<-c('AC', 'ACT', 'ACT', 'AD', 'AT', 'E', 'EP')
#VRI.1<- st_crop(VRI_raw,bbox(Lakes_large))
#VRI.1 <-vect(VRI_raw) %>%
#  crop(Lakes_large) %>%
#  sf::st_as_sf()
VRI_lks <-VRI_raw %>%
 mutate(VRI_id=as.numeric(rownames(.))) %>%
  mutate(PC_broadleaf=if_else(SPECIES_CD_1 %in% BroadlSp, SPECIES_PCT_1, 0)) %>%
           mutate(PC_broadleaf=if_else(SPECIES_CD_2 %in% BroadlSp, PC_broadleaf+SPECIES_PCT_2, PC_broadleaf)) %>%
                    mutate(PC_broadleaf=if_else(SPECIES_CD_3 %in% BroadlSp, PC_broadleaf+SPECIES_PCT_3, PC_broadleaf)) %>%
                             mutate(PC_broadleaf=if_else(SPECIES_CD_4 %in% BroadlSp, PC_broadleaf+SPECIES_PCT_4, PC_broadleaf)) %>%
                                      mutate(PC_broadleaf=if_else(SPECIES_CD_5 %in% BroadlSp, PC_broadleaf+SPECIES_PCT_5, PC_broadleaf)) %>%
                                               mutate(PC_broadleaf=if_else(SPECIES_CD_6 %in% BroadlSp, PC_broadleaf+SPECIES_PCT_6, PC_broadleaf)) %>%
  mutate(NVEG_PCT=NON_VEG_COVER_PCT_1+NON_VEG_COVER_PCT_2+NON_VEG_COVER_PCT_3) %>%
  dplyr::select_if(Negate(is.list)) #remove attributes that are a list #sapply(VRI_data, class)

write_sf(VRI_lks, file.path(spatialOutDir,"VRI_lks.gpkg"),layer_options = "OVERWRITE=true", 
         append=FALSE,delete_dsn=TRUE)

VRIr_lks_data <- VRI_lks %>%
  st_drop_geometry()
write.csv(VRIr_lks_data, file=file.path(dataOutDir,'VRIr_lks_data.csv'), row.names = FALSE)

#Write VRI with LUT
VRIr_lks<-VRI_lks  %>%
  #st_cast("MULTIPOLYGON") %>%
  fasterize(ProvRast,field='VRI_id')# %>%
  #terra::crop(AOI) %>%
  #terra::mask(AOI)
writeRaster(VRIr_lks,file.path(spatialOutDir,"VRIr_lks.tif"), format="GTiff", overwrite=TRUE)
#VRIr_lks<-raster(file.path(spatialOutDir,"VRIr_lks.tif"))

#Vtest<-VRI_data %>%
#  dplyr::filter(is.na(PC_broadleaf))

#Roads
roads<-st_read(file.path(spatialOutDir,"roads.gpkg"))
#Use Stars to rasterize according to RoadUse and save as a tif
#first st_rasterize needs a template to 'burn' the lines onto
BCr_S <- read_stars(file.path(spatialOutDir,'BCr_S.tif'), proxy=FALSE)
template = BCr_S
template[[1]][] = NA
roadsSR<-stars::st_rasterize(roads[,"RoadUse"], template)
roadsSR_Lks<-st_crop(roadsSR,st_bbox(AOI))
write_stars(roadsSR_Lks,dsn=file.path(spatialOutDir,'roadsSR_Lks.tif'))

#Disturbance
disturbance_Lks<-st_read(file.path(spatialOutDir,"disturbance_Lks.gpkg")) %>%
disturbance_Lksr<-disturbance_Lks %>% fasterize(ProvRast,field='disturb_Code') 

writeRaster(disturbance_Lksr,file.path(spatialOutDir,"disturbance_Lksr.tif"), format="GTiff", overwrite=TRUE)

} else {
  Lakes_TSAr<-raster(file.path(spatialOutDir,"Lakes_TSAr.tif"))
  Lakes_TSAbufr<-raster(file.path(spatialOutDir,"Lakes_TSAbufr.tif"))
  BECr<-raster(file.path(spatialOutDir,"BECr.tif"))
  BEC_data<-read.csv(file=file.path(dataOutDir,'BECr_data.csv'))
  VRI_Lks<-st_read(file.path(spatialOutDir,"VRI_Lks.gpkg"))
  VRI_data<-read.csv(file=file.path(dataOutDir,'VRIr_data.csv'))
  VRIr<-raster(file.path(spatialOutDir,"VRIr.tif"))
  roadsSR_Lks<-read_stars(file.path(spatialOutDir,'roadsSR_Lks.tif'))
  disturbance_Lks<-st_read(file.path(spatialOutDir,"disturbance_Lks.gpkg"))
  disturbance_Lksr<-raster(file.path(spatialOutDir,"disturbance_Lksr.tif"))
}

