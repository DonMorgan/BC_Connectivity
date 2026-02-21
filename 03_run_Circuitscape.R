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

#Set RunDir and global variables
RunDir<-"BCR_2_18Feb2026"
dir.create(file.path(ConnDir,RunDir), showWarnings = FALSE)
DirectionG<-list(c('North','South'),c('South','North'),c('East','West'),c('West','East'))
pixSize<-2

#Resistance surfaces come from HumanFootprint.R
#Resistance<-"resistance_surfaceP_NA"
#Resistance<-"resistance_surfaceP2"
#Resistance<-"resistance_surfaceP"
#Resistance<-"resistance_surfaceN"
Resistance<-"resistance_surfaceR"
#Resistance<-"Pither_resistance_surface"
#Resistance<-"resistance_surface_PASlp40"

#AOI_tile<-st_read(file.path(ConnDir,paste0('EcoPstrata.gpkg')))
#AOI_tile<-st_read(file.path(ConnDir,paste0('EcoRstrata.gpkg')))
AOI_tile<-BC

#Function to crop and aggregate the resistance layer
#needs AOI and pixel size
source ('crop_Resistance_function.R')


#Function to run circuitscape
source('CircuitScapeFn.R')
source('WallTile_function.R')
#Function to set up the input variables and call the crop_fn, WallTile_function and
#  CircuitScapeFn
#Cycles through each directional tile and runs circuitscape
# A wrapper would need to be put around this code to clyce through geographic tiles
CurMapTile_function<- function(i,pSize) {
  tile_no<-i
  AOI.1<- AOI_tile[i,]
  AOIBB <- st_bbox(AOI.1)
  #Make a bounding box x% larger than width
  AOIBuff<-round((AOIBB$xmax-AOIBB$xmin)*0.1,0)
  AOI<-AOI.1 %>%
    st_simplify(dTolerance = 1000) %>%
    st_buffer(dist=AOIBuff)
  #mapview(AOI)+mapview(AOI.1)
  #source("03_analysis_TileRun.R")
  resistance_surface_AOI<-crop_fn(AOI,pSize)
  #Generate the 4 wall to wall tiles
  #source('02_clean_WallTile.R')
  WallL<-WallTile_function(resistance_surface_AOI)
  #Loop through Directions for a given tile
  CurMapDirectionList<-lapply(1:length(DirectionG), function (j) {
   CircuitScape_function(j,tile_no)
  })
  return(CurMapDirectionList)
}

#Loop through each tile and generate a current map for each tile
  ClippedTileList<-lapply(1:nrow(AOI_tile), function (j) {
    CurrMap_DirectionTiles<-CurMapTile_function(j,pixSize)
    CurMapMean <- Reduce("mean", CurrMap_DirectionTiles)
    writeRaster(CurMapMean, filename=file.path(ConnDir,RunDir,paste0('CurMapMean_',j,'.tif')), overwrite=TRUE)
    })

#Join the tiles together
# clip to original tile AOI
  CurMapMeanL<-lapply(1:nrow(AOI_tile), function (j) {
    rast(file.path(ConnDir,RunDir,paste0('CurMapMean_',j,'.tif'))) %>%
      crop((AOI_tile[j,]),mask=TRUE)
  })
# merge the tiles into one map
  if (nrow(AOI_tile) > 1) {
  CurrMap<-do.call(terra::merge, CurMapMeanL)  %>%
    crop(BC,mask=TRUE)
  } else {
    CurrMap<-CurMapMeanL[[1]] %>%
      crop(BC,mask=TRUE)
  }
  writeRaster(CurrMap, filename=file.path(ConnDir,RunDir,paste0('CurMapMean_',RunDir,'.tif')), overwrite=TRUE)

