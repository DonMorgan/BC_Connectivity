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


crop_fn<-function(AOIin,piSize) {
  #crop the resistance surface
  #AOIin<-AOI
  resistance_surface_AOIin<-rast(file.path(SpatialDir,paste0(Resistance,".tif",sep=""))) %>%
    crop(AOIin,mask=TRUE)
  if (piSize>1) {
    print(paste0('PixSize=',piSize))
    resistance_surface_AOIin<-resistance_surface_AOIin %>%
      #For testing aggregate
      aggregate(fact=piSize,fun=modal,na.rm=TRUE) %>%
      crop(AOIin,mask=TRUE)
  }
  #Add a resistance of 1 where ever it is NA, check for subsequent runs....
  resistance_surface_AOIin[is.na(resistance_surface_AOIin)] <- 1
  writeRaster(resistance_surface_AOIin, filename=file.path(ConnDir,paste0('resistance_surface_AOI.tif')), overwrite=TRUE)
  return(resistance_surface_AOIin)
}
