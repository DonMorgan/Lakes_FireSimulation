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

#Load Union Fire gdb
Union_file <- file.path('tmp/UnionL')
if (!file.exists(Union_file)) {
  Union_gdb<-file.path(DataDir,'Fire/@Sept2019Unions/RO_ATU_VRI_BurnSev_Unions.gdb')
#Pull out fires of interest from gdb
Union_list<-as.data.frame(st_layers(Union_gdb)[[1]]) %>% 
  mutate(FireOfInterest=str_extract(.[[1]], 
     paste(FiresOfInterest, collapse="|"))) %>%
  dplyr::filter(!(is.na(FireOfInterest)))
#Creat a list of sf, one for each fire of interest
UnionL <- lapply(c(1:length(FiresOfInterest)), function(i)
  read_sf(Union_gdb, layer = Union_list[[1]][i]))
 names(UnionL)<-FiresOfInterest
} else {
UnionL<-readRDS(file='tmp/UnionL')
}

#Combine seperate fires into one file
fire.1<-UnionL[[FiresOfInterest[1]]] %>%
  #dplyr::select(-c(FID_R11796_burn_sev_2018_vri_2017_unioned,FID_R11796,                            
  #                 FID_R117_1)) %>%
  dplyr::select(-c(setdiff(names(.), names(UnionL[[FiresOfInterest[2]]])))) %>%
  mutate(FIRE_NAME=FiresOfInterest[1]) %>%
  mutate(Upoly_id=paste0(FIRE_NAME,'_',seq.int(nrow(.)))) %>%
  dplyr::select(order(colnames(.)))

NewUnionL<-lapply(c(2:length(UnionL)), function(i)
  UnionL[[FiresOfInterest[i]]] %>%
    mutate(FIRE_NAME=FiresOfInterest[i]) %>%
    mutate(Upoly_id=paste0(FIRE_NAME,'_',seq.int(nrow(.)))) %>%
    dplyr::select(colnames(fire.1)) %>%
    dplyr::select(order(colnames(.))))

Union_Fires.1<-rbind(fire.1,(do.call("rbind", NewUnionL)))
write_sf(Union_Fires.1, file.path(spatialOutDir,"Union_Fires.1.gpkg"))



#####################
#FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
#df_intersect<-intersect(names(df1), names(df2))
#df_diff<-setdiff(names(df1), names(df2))




