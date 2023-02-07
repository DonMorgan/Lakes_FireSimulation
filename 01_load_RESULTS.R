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


#RSLT_OPENING_SVW
# file.path(data,"RESULTS/RSLT_OPENING_SVW")

#RESULTS data
RESULTS_file=(file.path(spatialOutDir,'RESULTS.gpkg'))
if (!file.exists(RESULTS_file)) {
  #RSLT_OPENING_SVW
  RESULTS.RSLT_OPENING_SVW_gdb<-file.path(DataDir,'RESULTS/RSLT_OPENING_SVW/RSLT_OPENING_SVW.gdb')
  RESULTS.RSLT_OPENING_SVW_list <- st_layers(RESULTS.RSLT_OPENING_SVW_gdb)
  RESULTS.RSLT_OPENING_SVW_in <- read_sf(RESULTS.RSLT_OPENING_SVW_gdb, layer = "WHSE_FOREST_VEGETATION_RSLT_OPENING_SVW")
  saveRDS(RESULTS.RSLT_OPENING_SVW_in,file='tmp/RESULTS.RSLT_OPENING_SVW_in')
  write_sf(RESULTS.RSLT_OPENING_SVW_in, file.path(spatialOutDir,"RESULTS.RSLT_OPENING_SVW_in.gpkg"))
  #RSLT_ACTIVITY_TREATMENT_SVW
  RESULTS.RSLT_ACTIVITY_TREATMENT_SVW_gdb<-file.path(DataDir,'RESULTS/RSLT_ACTIVITY_TREATMENT_SVW/RSLT_ACTIVITY_TREATMENT_SVW.gdb')
  RESULTS.RSLT_ACTIVITY_TREATMENT_SVW_list <- st_layers(RESULTS.RSLT_ACTIVITY_TREATMENT_SVW_gdb)
  RESULTS.RSLT_ACTIVITY_TREATMENT_SVW_in <- read_sf(RESULTS.RSLT_ACTIVITY_TREATMENT_SVW_gdb, layer = "WHSE_FOREST_VEGETATION_RSLT_ACTIVITY_TREATMENT_SVW")
  saveRDS(RESULTS.RSLT_ACTIVITY_TREATMENT_SVW_in,file='tmp/RESULTS.RSLT_ACTIVITY_TREATMENT_SVW_in')
  write_sf(RESULTS.RSLT_ACTIVITY_TREATMENT_SVW_in, file.path(spatialOutDir,"RESULTS.RSLT_ACTIVITY_TREATMENT_SVW_in.gpkg"))
  #RSLT_FOREST_COVER_SILV_SVW
  RESULTS.RSLT_FOREST_COVER_SILV_SVW_gdb<-file.path(DataDir,'RESULTS/RSLT_FOREST_COVER_SILV_SVW/RSLT_FOREST_COVER_SILV_SVW.gdb')
  RESULTS.RSLT_FOREST_COVER_SILV_SVW_list <- st_layers(RESULTS.RSLT_FOREST_COVER_SILV_SVW_gdb)
  RESULTS.RSLT_FOREST_COVER_SILV_SVW_in <- read_sf(RESULTS.RSLT_FOREST_COVER_SILV_SVW_gdb, layer = "WHSE_FOREST_VEGETATION_RSLT_FOREST_COVER_SILV_SVW")
  saveRDS(RESULTS.RSLT_FOREST_COVER_SILV_SVW_in,file='tmp/RESULTS.RSLT_FOREST_COVER_SILV_SVW_in')
  write_sf(RESULTS.RSLT_FOREST_COVER_SILV_SVW_in, file.path(spatialOutDir,"RESULTS.RSLT_FOREST_COVER_SILV_SVW_in.gpkg"))
  
  VRI_in<-readRDS(file='tmp/VRI_in')
  write_sf(VRI_in, file.path(spatialOutDir,"VRI_in.gpkg"))
  RESULTS<-RESULTS_in %>%
    st_intersection(AOI) %>%
    st_collection_extract("POLYGON") %>%
    mutate(VRI_id=as.numeric(rownames(.)))
  write_sf(RESULTS, file.path(spatialOutDir,"RESULTS.gpkg"))

} else {
  RESULTS<-st_read(file.path(spatialOutDir,"RESULTS.gpkg")) 
 }
#nrow=unique(OBJECTID)
