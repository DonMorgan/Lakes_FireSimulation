# Copyright 2018 Province of British Columbia
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

options(timeout=180)
source("header.R")
source("packages.R")

FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")

#General load and clean
source('01_load.R') #general load - BARC, VRI, Lakes AOI, etc
source('02_clean.R')

#BARC scripts - RF
source('01_load_Fire_Union.R') #Fire union data set, spatial version of BARC?
source('02_clean_BARC.R') #original non-spatial BARC
source('03_analysis_RF.R') #RF analysis
source('03_analysis_map.R') #mapping RF analysis

#Weather
source('01_load_weather.R')
source('02_clean_weather.R')
source('03_analysis_Weather_IDW.R') #Generate weather maps using IDW
source('04_output_Weather_Rasters.R') #Clip to TSA

#DOB - Date of burn
source('01_load_DOB.R') #load DOB rasters from BVRC
source('02_clean_DOB_Rasters.R') #raster to vector then assign weather of day of burn to poly
source('03_analysis_DOB.R') #DOB data exploration- data distribution, etc.
source('03_analysis_DOB_burn_pattern.R') #Look for weather, FWI patterns in DOB - area burned by weather attribute

#Other loads
source('01_load_Fire_Rasters.R') #load rasters of individual fires from BVRC
source('01_load_Fire_Union.R') #load Fire 'union' spatial data set supplied by Carole Mahood 
source('01_load_VRI_Rasters.R') #load VRI rasters from BVRC


