CircuitScape_function<- function(i,tn) {
  print(paste0("tile No.",tn))
  print(paste0("direction No.",i))
  Direction_ini <- c("[Circuitscape mode]",
                     "data_type = raster",
                     "scenario = advanced",

                     "[Options for advanced mode]",
                     "ground_file_is_resistances = True",
                     #"remove_src_or_gnd = keepall",
                     paste0("ground_file =", file.path(ConnDir,paste0(DirectionG[[i]][2],'_T_ground.tif'))),
                     #"use_unit_currents = False",
                     paste0("source_file =", file.path(ConnDir,paste0(DirectionG[[i]][1],'_T_source.tif'))),
                     #"use_direct_grounds = False",

                     "[Calculation options]",
                     #"low_memory_mode = False",
                     "solver = cg+amg",
                     "print_timings = True",
                     #"print_rusages = True",#option not available
                     #"preemptive_memory_release = False",#option not available
                     #"parallelize = False",#option not available
                     #"max_parallel = 0",#option not available

                     "[Output options]",
                     "write_cur_maps = True",
                     #"write_cum_cur_map_only = False",
                     #"log_transform_maps = False",
                     #paste0("project_name = ",file.path(ConnDir,RunDir)),#option not available
                     #paste0("output_file =", file.path(ConnDir,RunDir,paste0(DirectionG[[i]][1],'2',DirectionG[[i]][2],'_tile_no_',tile_no,'.out'))),
                     paste0("output_file =", file.path(ConnDir,RunDir,paste0(DirectionG[[i]][1],'2',DirectionG[[i]][2],tn,'.out'))),
                     #"write_raw_currmap = True",#option not available
                     #"write_max_cur_maps = True",
                     #"write_volt_maps = False",
                     #"set_null_currents_to_nodata = False",
                     #"set_null_voltages_to_nodata = False",
                     #"compress_grids = False",
                     #"calc_normalized_current = True",#option not available
                     "write_as_tif = True",
                     #"set_focal_node_currents_to_zero = True",#option not available

                     "[Connection scheme for raster habitat data]",
                     "connect_using_avg_resistances = True",
                     "connect_four_neighbors_only = False",

                     "[Habitat raster or graph]",
                     "habitat_map_is_resistances = True",
                     paste0("habitat_file = ",file.path(ConnDir,"resistance_surface_AOI.tif")),
                     sep="/")

  #write ini file to disk at 'configLocation'
  configLocation<-file.path(ConnDir,"config.ini")
  cat(Direction_ini, sep="\n", file=configLocation)

  #write to jl file - that reads ini file and launches Omniscape on top of Julia
  script <- c('using Circuitscape',
              paste('compute(',configLocation,')',sep='"'))

  #Julia is happier if jl file is in directory that Julia is launched from
  cat(script, sep="\n", file="script.jl")

  #Set up parallel processing
  #Julia_Threads<-'export JULIA_NUM_THREADS=4'
  #system(Julia_Threads)
  #Launch Julia
  Julia_exe <- ('Julia script.jl')
  #Julia_exe <- ('/bin/bash julia script.jl')
  #Julia_exe <- ('compute("myjob.ini")')
  system(Julia_exe)

  #CurOut<-rast(paste0(file.path(ConnDir,RunDir,paste0(DirectionG[[i]][1],'2',DirectionG[[i]][2],'_tile_no_',tile_no,'_curmap.tif'))))
  CurOut<-rast(paste0(file.path(ConnDir,RunDir,paste0(DirectionG[[i]][1],'2',DirectionG[[i]][2],tn,'_curmap.tif'))))
  #plot(CurOut)

  return(CurOut)
}
