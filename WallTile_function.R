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


#Use the resistance surface as the AOI and set it's values to NA
#AOIBBr<-setValues(resistance_surface_AOI,NA)
#AOIBBr<-setValues(resistance_surface_AOI,-9999)
#names(AOIBBr)<-'NA_rast'

# Generate a bbox then convert to Terra raster
   #AOIBB <- st_as_sf(st_as_sfc(st_bbox(AOI)))
   # AOIBBr<-rast(AOIBB) %>%
   #   setValues(.,-9999) %>%
   #   extend(c(1,1))

  #Use the resistance surface as the AOI and set it's values to NA
#resistance_surface_AOI<-rast(file.path(ConnDir,paste0('resistance_surface_AOI.tif')))

WallTile_function<- function(Rsurface_AOI) {

  AOIBBr<-setValues(Rsurface_AOI,NA)
  #AOIBBr<-setValues(resistance_surface_AOI,-9999)
  names(AOIBBr)<-'NA_rast'

# Identify edge cells - fix is missing all the edge cells....
#edge_cells <- which(rows == 1 | rows == nrow(AOIBBr) | cols == 1 | cols == ncol(AOIBBr))
# Get row and column indices for the cells
rows <- rowFromCell(AOIBBr, 1:ncell(AOIBBr))
cols <- colFromCell(AOIBBr, 1:ncell(AOIBBr))

#Define ists of direction and for which side the 0 or 1 should be in the tile
#DirectionG<-c('North','South','East','West')
#val<-list(c(0,1),c(1,0),c(0,1),c(1,0))

#Generate a set of rasters of each cardinal direction one with 0 and the other 1
#North
  Ground <- AOIBBr
  Ground[which(rows == 1)] <- 0
  Source <- AOIBBr
  Source[which(rows == 1)] <- 1
  writeRaster(Ground,file.path(ConnDir,paste0('North_T_ground.tif')), overwrite=TRUE)
  writeRaster(Source,file.path(ConnDir,paste0('North_T_source.tif')), overwrite=TRUE)
  #East
  Ground <- AOIBBr
  Ground[which(cols == 1)] <- 0
  Source <- AOIBBr
  Source[which(cols == 1)] <- 1
  writeRaster(Ground,file.path(ConnDir,paste0('West_T_ground.tif')), overwrite=TRUE)
  writeRaster(Source,file.path(ConnDir,paste0('West_T_source.tif')), overwrite=TRUE)
  #South
  Ground <- AOIBBr
  Ground[which(rows == nrow(AOIBBr))] <- 0
  Source <- AOIBBr
  Source[which(rows == nrow(AOIBBr))] <- 1
  writeRaster(Ground,file.path(ConnDir,paste0('South_T_ground.tif')), overwrite=TRUE)
  writeRaster(Source,file.path(ConnDir,paste0('South_T_source.tif')), overwrite=TRUE)
  #West
  Ground <- AOIBBr
  Ground[which(cols == ncol(AOIBBr))] <- 0
  Source <- AOIBBr
  Source[which(cols == ncol(AOIBBr))] <- 1
  writeRaster(Ground,file.path(ConnDir,paste0('East_T_ground.tif')), overwrite=TRUE)
  writeRaster(Source,file.path(ConnDir,paste0('East_T_source.tif')), overwrite=TRUE)
  return(Ground)
  }

