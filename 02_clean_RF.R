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
#https://stackoverflow.com/questions/52860105/how-to-find-common-variables-in-different-data-frames

#Load data
Union_Fires<-st_read(file.path(spatialOutDir,"Union_Fires.gpkg"))

#strip out simple geo and data
Union_Fires.data<-Union_Fires %>%
  st_drop_geometry() 
write_csv(Union_Fires.data,file.path(dataOutDir,paste0('Union_Fires.data.csv')))

Union_Fires.geo<-Union_Fires %>%
  dplyr::select(Upoly_id)
write_sf(Union_Fires.geo, file.path(spatialOutDir,"Union_Fires.geo.gpkg"))

#join in dNBR data
#source('02_clean_NBR_Rasters.R')
UF.NBR.data<-read_csv(file.path(dataOutDir,paste0('UF.NBR.data.data.csv')))

range(UF.NBR.data$dNBR,na.rm = TRUE)
range(UF.NBR.data$dNBR_R11796,na.rm = TRUE)
range(UF.NBR.data$dNBR_R11498,na.rm = TRUE)
range(UF.NBR.data$dNBR_R21721,na.rm = TRUE)
range(UF.NBR.data$dNBR_R11921,na.rm = TRUE)
range(UF.NBR.data$dNBR_G41607,na.rm = TRUE)
range(UF.NBR.data$dNBR_G51632,na.rm = TRUE)

#join in the HLI data
#source('03_analysis_HLI.R')
UF.HL<-st_read(file.path(spatialOutDir,"UF.HL.v.gpkg")) %>%
  st_drop_geometry() %>%
  dplyr::rename(HLI=lyr.1) %>%
  dplyr::select(Upoly_id,HLI)

#Rebuild Union Fires data set
UF.1<-UF.NBR.data %>%
  left_join(Union_Fires.data, by='Upoly_id') %>%
  dplyr::select(Upoly_id,starts_with("dNBR")) %>%
  left_join(UF.HL, by='Upoly_id')

UF.2<-UF.1 %>%
  left_join(Union_Fires.data, by='Upoly_id')

#Classify burn severity
# unburned dNBR <0.1; 
# low severity dNBR 0.1 to 0.269; 
# moderate severity dNBR 0.27 to 0.659; 
# high severity dNBR >0.659
# Depending on how you calculate the dNBR values, those numbers might have to be scaled by 1000?

UF.3<-UF.2 %>%
  st_drop_geometry() %>%
  mutate(BurnSevCalc=case_when(
    dNBR.mean>=0.659 ~ 'high',
    dNBR.mean>=0.27 & dNBR.mean  ~ 'mod',
    dNBR.mean>=0.1 ~ 'low', 
    dNBR.mean<0.1 ~ 'unburned',
    TRUE ~ 'unknown')) %>%
# Broadleaf leading stands (PC_broadleaf>50): no-low threshold, dNBR 0.1954; low-moderate threshold, dNBR 0.3867; mod-high threshold, dNBR 0.8246
  mutate(BurnSevCalc2=case_when(
    PC_broadleaf>50 & dNBR.mean>=0.8246 ~ 'high',
    PC_broadleaf>50 & dNBR.mean>=0.3867 ~ 'mod',
    PC_broadleaf>50 & dNBR.mean>=0.1954 ~ 'low',
    PC_broadleaf>50 & dNBR.mean<0.1954 ~ 'unburned',
    TRUE ~ BurnSevCalc)) %>%
# Plantations >20yrs old (cutblock==historic): no-low threshold, dNBR.mean 0.0128; low-mod threshold, dNBR.mean 0.2159; mod-high threshold, dNBR.mean 0.6807
  mutate(BurnSevCalc2=case_when(
    cutblock=='historic' & dNBR.mean>=0.8246 ~ 'high',
    cutblock=='historic' & dNBR.mean>=0.2159 ~ 'mod',
    cutblock=='historic' & dNBR.mean>=0.0128 ~ 'low',
    cutblock=='historic' & dNBR.mean<0.0128 ~ 'unburned',
    TRUE ~ BurnSevCalc)) %>%
# Natural conifer-leading stands, all ages (logged==0 & (PC_broadleaf<50 & STAND_AGE>0)): no-low threshold, dNBR.mean 0.0287; low-mod threshold, dNBR.mean 0.1929; mod-high threshold, dNBR.mean 0.5687
  mutate(BurnSevCalc2=case_when(
    (logged==0 & (PC_broadleaf<50 & STAND_AGE>0)) & dNBR.mean>=0.5687 ~ 'high',
    (logged==0 & (PC_broadleaf<50 & STAND_AGE>0)) & dNBR.mean>=0.1929 ~ 'mod',
    (logged==0 & (PC_broadleaf<50 & STAND_AGE>0)) & dNBR.mean>=0.0287 ~ 'low',
    (logged==0 & (PC_broadleaf<50 & STAND_AGE>0)) & dNBR.mean<0.0287 ~ 'unburned',
    TRUE ~ BurnSevCalc)) 

table(UF.3$BurnSev,UF.3$BurnSevCalc2)

UF.3.geo<-Union_Fires.geo %>%
  left_join(UF.3) %>%
  dplyr::select(Upoly_id,BurnSev,BurnSevCalc,BurnSevCalc2,dNBR.mean,dNBR.max, logged, 
                PC_broadleaf,STAND_AGE,cutblock)
write_sf(UF.3.geo, file.path(spatialOutDir,"UF.3.geo.gpkg"))

UF.4<-UF.3 %>%
  dplyr::select(BurnSev,BurnSevCalc,BurnSevCalc2,dNBR, logged, 
                PC_broadleaf,STAND_AGE,cutblock) 

%>%
  dplyr::filter(BurnSevCalc != BurnSevCalc2)

message('Breaking')
break


##############

#Select attributes for RF model
RFdata<-Union_Fires %>%
  dplyr::select(FireNum,FireYear,FIRE_NAME,
                POLY_ID,Area_Ha, BurnSev, logged,
                PC_broadleaf,SHRB_CC,STAND_AGE,NVEG_PCT,WTED_PROJ_HT,PC_DEAD,CR_CLOSURE,
                LEADING_SPP_COV,TOT_STEMS,SITE_INDEX,LIVE_STEMS,
                VERT_COMPLEXITY,TREE_PATTERN)


