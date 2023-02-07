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

#read in source data
BARC_in<-read_csv(file.path(DataDir,'six_2018_fires_BARCxVRI.csv')) 


BARC_wt<-BARC_in %>%
       dplyr::select(Area_Ha,LEADING_SPP_COV,COV_PCT_1,SPEC_PCT_1,P_AGE_CAS1,PC_DEAD) 

%>%
  mutate(TOT_STEMS.1=DEAD_STEMS+LIVE_STEMS) %>%
  mutate(PC_DEAD.1=DEAD_STEMS/TOT_STEMS.1*100)
  
  #height weighted by each cohort
  

BARC_wt<-BARC_in %>%
  dplyr::select(LEADING_SPP_COV,SPEC_PCT_1,COV_PCT_1)

                %>%
  mutate(SA=STAND_AGE-PROJ_AGE_1)


BARC_in %>%
  dplyr::count(FireYear)
#Look at some VRI attributes
B_VRI<-BARC_in %>%
 dplyr::select(FID_R11796,FireNum,FireYear,BurnSev,PreImg,PreImgDate,PstImg,
               PstImgDate,Area_Ha,Shape_Area,POLY_AREA,Comments,OK,FID_G416_1,FEATURE_ID,MAP_ID,POLY_ID,OPEN_IND)
View(B_VRI)
 
#Check POLY_ID
B_VRI_poly<-BARC_in %>% 
  dplyr::select(POLY_ID,FireNum,FID_R11796,FID_R11498,FID_R21721,FID_R11921,FID_G41607,FID_G51632) %>%
  dplyr::filter(POLY_ID==12585966)
View(B_VRI_poly)

#Select attributes used for BARC analysis to check
B_check<-BARC_in %>%
  #filter data to look at single fire R11796
  dplyr::filter(!(is.na(FID_R11796))) %>%
  dplyr::select(FID_R11796,FireNum,FireYear,Area_Ha, PreImg,PreImgDate, 
                PstImg,PstImgDate,Comments, OK, POLY_ID, BurnSev, logged)

#Select attributes for RF model
B_RFdata<-BARC_in %>%
   dplyr::select(FireNum,FireYear,FIRE_NAME,
                 FID_R11796,FID_R11498,FID_R21721,FID_R11921,FID_G41607,FID_G51632,
                 POLY_ID,Area_Ha, BurnSev, logged,
                 PC_broadleaf,SHRB_CC,STAND_AGE,NVEG_PCT,WTED_PROJ_HT,PC_DEAD,CR_CLOSURE,
                 LEADING_SPP_COV,TOT_STEMS,SITE_INDEX,LIVE_STEMS,
                 VERT_COMPLEXITY,TREE_PATTERN)


#NA options - NA undermines Random Forest classification
#Set all NA to 0
#  BARC2[is.na(BARC2)] <- 0

#better to assign value individually, as well some attributes need to be rationalized
#Clean up NA in data 
B_RFdata <- BARC_in %>%
  dplyr::select(FireNum,FireYear,FIRE_NAME,
                FID_R11796,FID_R11498,FID_R21721,FID_R11921,FID_G41607,FID_G51632,
                POLY_ID,Area_Ha, BurnSev, logged,
                PC_broadleaf,SHRB_CC,STAND_AGE,NVEG_PCT,WTED_PROJ_HT,PC_DEAD,CR_CLOSURE,
                LEADING_SPP_COV,TOT_STEMS,SITE_INDEX,LIVE_STEMS,
                VERT_COMPLEXITY,TREE_PATTERN) %>%
  #set NA PC_broadleaf to SHRB_CC since shrubs are broad leaf
  mutate(PC_broadleaf=if_else(is.na(PC_broadleaf) | PC_broadleaf<SHRB_CC, SHRB_CC, PC_broadleaf)) %>%
  #Set STAND_AGE to 0 if typed in a non veg unit ie there is no forest, all others are NA and set above
  mutate(STAND_AGE=if_else(NVEG_PCT==100,0,STAND_AGE)) %>%
  #VERT_COMPLEXITY -is.na and STAND_AGE>0 should be typed, else 'none'
  mutate(VERT_COMPLEXITY=case_when(is.na(VERT_COMPLEXITY) & STAND_AGE>0 ~ '3', #most common - modal(BARC_in$VERT_COMPLEXITY,na.rm=TRUE)
                                   is.na(VERT_COMPLEXITY) & STAND_AGE==0 ~ 'none',
                                   TRUE ~  as.character(VERT_COMPLEXITY))) %>%
  #TREE_PATTERN is.na and STAND_AGE>0 should be typed, else 'none'
  mutate(TREE_PATTERN=case_when(is.na(TREE_PATTERN) & STAND_AGE>0 ~ '8', #modal(BARC_in$TREE_PATTERN,na.rm=TRUE)
                                 is.na(TREE_PATTERN) & STAND_AGE==0 ~ 'none',
                                 TRUE ~  as.character(TREE_PATTERN)))
#Set WTED_PROJ_HT to 0 if NA  other option < min ht(0.1) -min(B_check$WTED_PROJ_HT, na.rm=TRUE)
B_RFdata$WTED_PROJ_HT[is.na(B_RFdata$WTED_PROJ_HT)] <- 0
#Set PC_DEAD to 0 if NA - non vri?0 TOT_STEMS-LIVE_STEMS/TOT_STEMS
B_RFdata$PC_DEAD[is.na(B_RFdata$PC_DEAD)] <- 0

View(B_RFdata)
table(B_RFdata$BurnSev)

#Data explore
R11796 <- B_RFdata %>%
  dplyr::filter(!(is.na(FID_R11796)))
  
length(unique(R11796$POLY_ID))


B_na<-B_RFdata %>% 
  dplyr::filter(is.na(TREE_PATTERN))
#dplyr::filter(is.na(WTED_PROJ_HT))

B_NVEG<-B_RFdata %>% 
  dplyr::filter(NVEG_PCT>98)

B_broad<-B_RFdata %>% 
  dplyr::filter(PC_broadleaf>0)

B_high<-B_RFdata %>% 
  dplyr::filter(BurnSev=='H')

B_med<-B_RFdata %>% 
  dplyr::filter(BurnSev=='M')


  

