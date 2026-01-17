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

source('header.R')

source("01_load.R")
#repo BC_HumanFootprint generates a weighted surface based on Pither et al 2023
#uses Provincial CE disturbance layer and consolidated roads
#file should be in the 'SpatialDir' directory and named 'resistance_surface'

#run Wall to Wall
# set pixel size (for testing), and define result directory
# modify 03_run_Circuitscape.R to run analysis

#run Omniscape
# set run directory for omniscape
# modify 03_run_Omniscape.R to run analysis



