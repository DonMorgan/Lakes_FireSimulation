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


Union_Fires.1<-st_read(file.path(spatialOutDir,"Union_Fires.1.gpkg"))

#Use HARV_DATE to classify logged stands and to determine new vs old cutblocks
BroadlSp<-c('AC', 'ACT', 'ACT', 'AD', 'AT', 'E', 'EP')

Union_Fires.2<-Union_Fires.1 %>%
  mutate(logged=ifelse(!(is.na(HARV_DATE)),1,0)) %>%
  mutate(cutblock=case_when(
    logged==1 & (HARV_DATE < ymd_hms("2003-01-01 01:00:00")) ~ 'historic',
    logged==1 & (HARV_DATE > ymd_hms("2003-01-01 01:00:00")) ~ 'current',
    TRUE ~ '')) %>%
  mutate(STAND_AGE=(PROJ_AGE_1+1)) %>% #age of dominant stand
  mutate(WTED_PROJ_HT=PROJ_HT_1*SPEC_PCT_1/(SPEC_PCT_1+SPEC_PCT_2)+PROJ_HT_2*SPEC_PCT_2/(SPEC_PCT_1+SPEC_PCT_2)) %>% #height weighted by each cohort
  mutate(NVEG_PCT.1=NVEG_PCT_1+NVEG_PCT_2+NVEG_PCT_3) %>% #height weighted by each cohort
  mutate(PC_broadleaf=if_else(SPEC_CD_1 %in% BroadlSp, SPEC_PCT_1, 0)) %>%
  mutate(PC_broadleaf=if_else(SPEC_CD_2 %in% BroadlSp, PC_broadleaf+SPEC_PCT_2, PC_broadleaf)) %>%
  mutate(PC_broadleaf=if_else(SPEC_CD_3 %in% BroadlSp, PC_broadleaf+SPEC_PCT_3, PC_broadleaf)) %>%
  mutate(PC_broadleaf=if_else(SPEC_CD_4 %in% BroadlSp, PC_broadleaf+SPEC_PCT_4, PC_broadleaf)) %>%
  mutate(PC_broadleaf=if_else(SPEC_CD_5 %in% BroadlSp, PC_broadleaf+SPEC_PCT_5, PC_broadleaf)) %>%
  mutate(PC_broadleaf=if_else(SPEC_CD_6 %in% BroadlSp, PC_broadleaf+SPEC_PCT_6, PC_broadleaf)) %>%
  mutate(TOT_STEMS=DEAD_STEMS+LIVE_STEMS) %>%
  mutate(PC_DEAD=DEAD_STEMS/TOT_STEMS*100) %>%
  mutate(NVEG_PCT=NVEG_PCT_1+NVEG_PCT_2+NVEG_PCT_3) %>%
  mutate(LEADING_SPP_COV=SPEC_PCT_1) 

Union_Fires<-Union_Fires.2 %>%
  #set NA PC_broadleaf to SHRB_CC since shrubs are broad leaf
  mutate(PC_broadleaf=if_else(is.na(PC_broadleaf) | PC_broadleaf<SHRB_CC, as.numeric(SHRB_CC), PC_broadleaf)) %>%
  #Set STAND_AGE to 0 if typed in a non veg unit ie there is no forest, all others are NA and set above
  mutate(STAND_AGE=if_else(NVEG_PCT==100,0,STAND_AGE)) %>%
  #VERT_COMPLEXITY -is.na and STAND_AGE>0 should be typed, else 'none'
  mutate(VERT_COMPL=case_when(is.na(VERT_COMPL) & STAND_AGE>0 ~ '3', #most common - modal(BARC_in$VERT_COMPLEXITY,na.rm=TRUE)
                                   is.na(VERT_COMPL) & STAND_AGE==0 ~ 'none',
                                   TRUE ~  as.character(VERT_COMPL))) %>%
  #TREE_PATTERN is.na and STAND_AGE>0 should be typed, else 'none'
  mutate(TREE_PATRN=case_when(is.na(TREE_PATRN) & STAND_AGE>0 ~ '8', #modal(BARC_in$TREE_PATRN,na.rm=TRUE)
                                is.na(TREE_PATRN) & STAND_AGE==0 ~ 'none',
                                TRUE ~  as.character(TREE_PATRN)))
#Set WTED_PROJ_HT to 0 if NA  other option < min ht(0.1) -min(B_check$WTED_PROJ_HT, na.rm=TRUE)
Union_Fires$WTED_PROJ_HT[is.na(Union_Fires$WTED_PROJ_HT)] <- 0
#Set PC_DEAD to 0 if NA - non vri?0 TOT_STEMS-LIVE_STEMS/TOT_STEMS
Union_Fires$PC_DEAD[is.na(Union_Fires$PC_DEAD)] <- 0

write_sf(Union_Fires, file.path(spatialOutDir,"Union_Fires.gpkg"))



