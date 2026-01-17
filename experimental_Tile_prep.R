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

#Load and clean layers to be explored as tiling options if required to increase processing speed

#Rationalize EcoRegions to partition Province for analysis
EcoProvinces<-bcmaps::ecoprovinces()
EcoRegions<-bcmaps::ecoregions()

EcoProvincesL.1<-c('SBI','SIM','SOI','CEI')
EcoPjoin1<-c('BOP','TAP')
NBM_JL<-c('HHI','LIB','PEM','YSL','BMP','NRM')
COM_SL<-c('HES','OPS','HCS','EVI','HIC','COM','GWH','WVI','PAC','COG','IPS','GBH','GED','GPB','LOM')
COM_NL<-c('STE','CMI','BOI','YSH','BOU','NRA')

EcoP_1<-EcoProvinces %>%
  dplyr::filter(ECOPROVINCE_CODE %in% EcoProvincesL.1) %>%
  mutate(Group=1) %>%
  dplyr::select(Group)

COM_J<-EcoProvinces %>%
  dplyr::filter(ECOPROVINCE_CODE %in% EcoPjoin1) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(Group=1) %>%
  st_set_geometry("geometry") %>%
  dplyr::select(Group)

NBM_J<-EcoRegions %>%
  dplyr::filter(ECOREGION_CODE %in% NBM_JL) %>%
  mutate(Group=1) %>%
  group_by(Group) %>%
  dplyr::summarise(geometry=st_union(geometry)) %>%
  dplyr::select(Group)

COM_S<-EcoRegions %>%
  dplyr::filter(ECOREGION_CODE %in% COM_SL) %>%
  mutate(Group=1) %>%
  group_by(Group) %>%
  dplyr::summarise(geometry=st_union(geometry)) %>%
  dplyr::select(Group)

COM_N<-EcoRegions %>%
  dplyr::filter(ECOREGION_CODE %in% COM_NL) %>%
  mutate(Group=1) %>%
  group_by(Group) %>%
  dplyr::summarise(geometry=st_union(geometry)) %>%
  st_cast("POLYGON") %>%
  dplyr::select(Group)

EcoProvincesL<-list(EcoP_1,COM_J,NBM_J,COM_S,COM_N)
EcoPstrata<-bind_rows(EcoProvincesL)
write_sf(EcoPstrata, file.path(ConnDir,paste0('EcoPstrata.gpkg')))

mapview(EcoP_1)+mapview(NBM_J)+mapview(COM_J)+mapview(COM_S)+mapview(COM_N)+EcoProvinces+EcoRegions

#Use EcoRegions to divide Province into south and north for analysis
EcoRegions<-bcmaps::ecoregions()

N_EcoRegionsL<-c('NUP','HSL','MPL','CAU','BUP','SAU','ECR','CRM','NRM','HHI','LIB','PRB',
                 'PEM','BMP','OMM','FAB','SKM','YSH','YSL','STE','CMI','BOU','NRA')

N_EcoRegions<-EcoRegions %>%
  dplyr::filter(ECOREGION_CODE %in% N_EcoRegionsL) %>%
  mutate(Group=1) %>%
  group_by(Group) %>%
  summarise(geometry=st_union(geometry))

S_EcoRegions<-EcoRegions %>%
  dplyr::filter((!ECOREGION_CODE %in% N_EcoRegionsL) & !ECOREGION_CODE %in% c("SBC","TPC")) %>%
  mutate(Group=1) %>%
  group_by(Group) %>%
  summarise(geometry=st_union(geometry))

mapview(N_EcoRegions)+mapview(S_EcoRegions)

EcoRegionsL<-list(N_EcoRegions,S_EcoRegions)
EcoRstrata<-bind_rows(EcoRegionsL)
write_sf(EcoRstrata, file.path(ConnDir,paste0('EcoRstrata.gpkg')))



