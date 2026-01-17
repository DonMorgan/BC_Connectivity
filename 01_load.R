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

#Rasterize the Province for subsequent masking
# bring in BC boundary
bc <- bcmaps::bc_bound()
Prov_crs<-crs(bc)

#Provincial Raster to place rasters in the same reference
BCr_file <- file.path(spatialOutDir,"BCr.tif")
if (!file.exists(BCr_file)) {
  BC<-bcmaps::bc_bound_hres(class='sf')
  saveRDS(BC,file='tmp/BC')
  BC_wbuff<-BC %>%
    st_buffer(dist=100000)
  mapview(BC)+mapview(BC_wbuff)
  saveRDS(BC,file='tmp/BC')
  BCr <- fasterize(bcmaps::bc_bound_hres(class='sf'),ProvRast)
  writeRaster(BCr, filename=BCr_file, format="GTiff", overwrite=TRUE)
  ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                   ymn=173787.5, ymx=1748187.5,
                   crs=Prov_crs,
                   res = c(100,100), vals = 1)
  writeRaster(ProvRast, filename=file.path(spatialOutDir,'ProvRast'), format="GTiff", overwrite=TRUE)
} else {
  BCr <- raster(BCr_file)
  BC <-readRDS('tmp/BC')
  ProvRast<-raster(file.path(spatialOutDir,'ProvRast.tif'))
}

#Rationalize EcoRegions to partition Province for analysis
#Reproject Pither cost layer
Pither_resistance_surface<-rast(file.path(SpatialDir,'Pither_Movement_Cost_Layer.tif')) %>%
  project(Prov_crs,method='near') #compare with max?
writeRaster(Pither_resistance_surface,file.path(SpatialDir,'Pither_resistance_surface.tif'),overwrite=TRUE)



