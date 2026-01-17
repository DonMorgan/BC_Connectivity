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

#Set Run variables for Ominscape
#Set RunDir
pixSize<-1 #
RunDir<-"T_Test10"
dir.create(file.path(ConnDir,RunDir), showWarnings = FALSE)

#Resistance surfaces come from HumanFootprint.R
#Resistance<-"resistance_surfaceP_NA"
#Resistance<-"resistance_surfaceP"
#Resistance<-"resistance_surfaceN"
#Resistance<-"resistance_surfaceR"
#Resistance<-"Pither_resistance_surface"
Resistance<-"resistance_surface_PASlp40"

#Test AOIs
#AOI_tile<-st_read(file.path(ConnDir,paste0('EcoPstrata.gpkg')))
#AOI_tile<-st_read(file.path(ConnDir,paste0('EcoRstrata.gpkg')))
#Select AOI
#AOI<-st_read(file.path(spatialOutDir,"ws.gpkg")) %>%
#  dplyr::filter(SUB_SUB_DRAINAGE_AREA_NAME=='Bulkley')
#AOI<-st_read(file.path(spatialOutDir,"EcoP.gpkg")) %>%
#  dplyr::filter(ECOPROVINCE_NAME=='CENTRAL INTERIOR')
AOI_tile<-BC

# A wrapper would need to be put around this code to cycle through geographic tiles

#Function to crop and aggregate the resistance layer
#needs AOI and pixel size, returns cropped resistance surface
source ('crop_Resistance_function.R')
#needs AOI and pixel size, returns cropped source surface
source ('crop_Source_function.R')

source("03_Omni_analysis.R")


