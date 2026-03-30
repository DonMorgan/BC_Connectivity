#####
#R script from Carroll et al. (2018)
#https://onlinelibrary.wiley.com/doi/10.1111/gcb.14373


##R script for calculating curren-flow and shortest-path centrality based on current and future climate types. 
##From Carroll et al. in review "Climatic, topographic, and anthropogenic factors determine connectivity between current and future climate analogs in North America".
##Please see http://adaptwest.databsin.org for updates and data downloads.

##1. Generate input files used in production of source and target points and transition layer for each climate type
library(raster)
url <- "https://download.r-forge.r-project.org/bin/windows/contrib/4.4/rgdal_1.6-7.zip"
install.packages(url, type="source", repos=NULL)
library(rgdal)
library(rgeos)
library(gdistance)
library(parallel)
library(doParallel)
library(fasterize)
library(sf)

cp <- 2 # cost penalty: number cost units per climate bin dissimilarity

#mask <- raster('mask.tif') #mask delineates area of analysis
mask <- raster("E:/Don_Gen_connectivity/Spatial_data_from_Don/BCrBuff_1km2_omit_ocean.tif")
#mask[mask <= 1000] <- 1
#lakes <- raster('lakes.tif')
#mask[lakes == 1] <- NA

t1.pc1 <- raster("E:/Don_Gen_connectivity/Climate_data/AdaptWest/Normal_1991_2020_bioclim/PCA/pca_11_axc1_bc_1km2.tif")
names(t1.pc1) <- 't1.pc1'
t1.pc1.min <- minValue(t1.pc1); t1.pc1.max <- maxValue(t1.pc1)
t1.pc1 <- (100-1) / (t1.pc1.max-t1.pc1.min) * (t1.pc1 - t1.pc1.min) + 1
t1.pc1[is.na(mask)] <- NA
t1extent<-extent(t1.pc1)

t1.pc2 <- raster("E:/Don_Gen_connectivity/Climate_data/AdaptWest/Normal_1991_2020_bioclim/PCA/pca_11_axc2_bc_1km2.tif")
names(t1.pc2) <- 't1.pc2'
t1.pc2.min <- minValue(t1.pc2); t1.pc2.max <- maxValue(t1.pc2)
t1.pc2 <- (100-1) / (t1.pc2.max-t1.pc2.min) * (t1.pc2 - t1.pc2.min) + 1
t1.pc2[is.na(mask)] <- NA

t33.pc1 <- raster("E:/Don_Gen_connectivity/Climate_data/AdaptWest/ensemble_8GCMs_ssp245_2041_2070/ensemble_8GCMs_ssp245_2041_2070_bioclim/PCA/pca_11_axc1_bc_1km2.tif")
names(t33.pc1) <- 't33.pc1'
t33.pc1 <- (100-1) / (t1.pc1.max-t1.pc1.min) * (t33.pc1 - t1.pc1.min) + 1
t33.pc1[is.na(mask)] <- NA

t33.pc2 <- raster("E:/Don_Gen_connectivity/Climate_data/AdaptWest/ensemble_8GCMs_ssp245_2041_2070/ensemble_8GCMs_ssp245_2041_2070_bioclim/PCA/pca_11_axc2_bc_1km2.tif")
names(t33.pc2) <- 't33.pc2'
t33.pc2 <- (100-1) / (t1.pc2.max-t1.pc2.min) * (t33.pc2 - t1.pc2.min) + 1
t33.pc2[is.na(mask)] <- NA

x.min <- xmin(t1.pc1)
x.max <- xmax(t1.pc1)
y.min <- ymin(t1.pc1)
y.max <- ymax(t1.pc1)

resistance.mask <- raster("E:/Don_Gen_connectivity/Spatial_data_from_Don/resistance_surfaceP_extr.tif")

pc1.stack <- stack(t1.pc1)
for (r in 2:32) {
  the.raster <- setValues(t1.pc1,NA)
  names(the.raster) <- paste('t', r, '.pc1', sep='')
  pc1.stack <- addLayer(pc1.stack, the.raster)
}
pc1.stack <- addLayer(pc1.stack, t33.pc1)
pc1.stack <- approxNA(pc1.stack)      # linearly interpolate across stack


pc2.stack <- stack(t1.pc2)
for (r in 2:32) {
  the.raster <- setValues(t1.pc2,NA)
  names(the.raster) <- paste('t', r, '.pc2', sep='')
  pc2.stack <- addLayer(pc2.stack, the.raster)
}
pc2.stack <- addLayer(pc2.stack, t33.pc2)
pc2.stack <- approxNA(pc2.stack)      # linearly interpolate across stack

# Start: create reclass matrix
bin.df <- as.data.frame(matrix(nrow=12600, ncol=7)); colnames(bin.df) <- c('bin.num', 'pc1', 'pc2', 'pc1.min', 'pc1.max', 'pc2.min', 'pc2.max')
bin.df$bin.num <- seq(1, nrow(bin.df))
bin.df$pc1 <- rep(-4:100, each=120)
bin.df$pc2 <- seq(1, 120)

bin.size <- 1
offset<-0 # Offset can be set to a series e.g. seq(0,0.1,0.2) and the script run multiple times to reduce bin edge effects

for (i in 1:nrow(bin.df)) {
  
  bin.df[i, 'pc1.min'] <- bin.df[i, 'pc1'] - (bin.size/2) + offset
  bin.df[i, 'pc1.max'] <- bin.df[i, 'pc1'] + (bin.size/2) + offset
  
  bin.df[i, 'pc2.min'] <- bin.df[i, 'pc2'] - (bin.size/2) + offset
  bin.df[i, 'pc2.max'] <- bin.df[i, 'pc2'] + (bin.size/2) + offset
}
# End: create reclass matrix


xy <- as.data.frame(xyFromCell(t1.pc1, 1:ncell(t1.pc1)))

xy$pc1.current <- (extract(t1.pc1, xy[,c('x','y')])); xy <- na.omit(xy)
xy$pc2.current <- (extract(t1.pc2, xy[,c('x','y')]))
xy$pc1.future <- (extract(t33.pc1, xy[,c('x','y')]))
xy$pc2.future <- (extract(t33.pc2, xy[,c('x','y')]))

#2. Define main function for production of source and target points and transition layer for each climate type

main.function <- function(bin) {
  
  out.df.final <- as.data.frame(matrix(nrow=0, ncol=4))
  colnames(out.df.final) <- c("from.x","from.y","to.x","to.y")
  
  no.change.df.final <- as.data.frame(matrix(nrow=0, ncol=4))
  colnames(no.change.df.final) <- c("from.x","from.y","to.x","to.y") 
  
  no.analog.df.final <- as.data.frame(matrix(nrow=0, ncol=2))
  colnames(no.analog.df.final) <- c("x","y") 
  
  pc1.center <- bin.df[bin,]$pc1
  pc2.center <- bin.df[bin,]$pc2
  
  pc1.min <- bin.df[bin,]$pc1.min
  pc1.max <- bin.df[bin,]$pc1.max
  pc2.min <- bin.df[bin,]$pc2.min
  pc2.max <- bin.df[bin,]$pc2.max
  
  from.xy <- subset(xy, pc1.current >= pc1.min &  pc1.current < pc1.max)
  from.xy <- subset(from.xy, pc2.current >= pc2.min &  pc2.current < pc2.max)
  
  to.xy <- subset(xy, pc1.future >= pc1.min &  pc1.future < pc1.max)
  to.xy <- subset(to.xy, pc2.future >= pc2.min &  pc2.future < pc2.max)
  
  if (nrow(from.xy) > 0 & nrow(to.xy) > 0) {
    
    for (kk in 1:33) {
      
      t.pc1 <- pc1.stack[[kk]] 
      pc1.rr <- abs(t.pc1 - pc1.center) * cp
      pc1.rr[(t.pc1 >= (pc1.min)) & (t.pc1 < (pc1.max))] <- 0
      
      t.pc2 <- pc2.stack[[kk]] 
      pc2.rr <- abs(t.pc2 - pc2.center) * cp
      pc2.rr[(t.pc2 >= (pc2.min)) & (t.pc2 < (pc2.max))] <- 0
      
      assign(paste('t', kk, '.rr', sep=''), (pc1.rr + pc2.rr) + 0.1)
    }
    
    resistance.raster <- min(t1.rr, t2.rr, t3.rr, t4.rr, t5.rr, t6.rr, t7.rr, t8.rr, t9.rr, t10.rr, t11.rr, t12.rr, t13.rr, t14.rr, t15.rr, t16.rr, 
                             t17.rr, t19.rr, t20.rr, t21.rr, t22.rr, t23.rr, t24.rr, t25.rr, t26.rr, t27.rr, t28.rr, t29.rr, t30.rr, t31.rr, t32.rr, t33.rr)
    from.x.min <- min(from.xy$x) - 1400000
    from.x.max <- max(from.xy$x) + 1400000
    from.y.min <- min(from.xy$y) - 1400000
    from.y.max <- max(from.xy$y) + 1400000
    
    to.x.min <- min(to.xy$x) - 1400000
    to.x.max <- max(to.xy$x) + 1400000
    to.y.min <- min(to.xy$y) - 1400000
    to.y.max <- max(to.xy$y) + 1400000
    
    new.x.min <- min(from.x.min, to.x.min)
    new.x.min <- max(new.x.min, x.min)
    new.x.max <- max(from.x.max, to.x.max)
    new.x.max <- min(new.x.max, x.max)
    
    new.y.min <- min(from.y.min, to.y.min)
    new.y.min <- max(new.y.min, y.min)
    new.y.max <- max(from.y.max, to.y.max)
    new.y.max <- min(new.y.max, y.max)
    
    new.extent <- extent(c(new.x.min, new.x.max, new.y.min, new.y.max))
    
    resistance.raster <- crop(resistance.raster, new.extent)
    resistance.mask.tmp <- crop(resistance.mask, new.extent)
    resistance.raster[is.na(resistance.raster == T) & resistance.mask.tmp == 1] <- 5000 # change water to resistance=5000
    
    f <- function(x) 1/mean(x)
    trans <- transition(resistance.raster, transitionFunction=f, directions=8) 
    trans <- geoCorrection(trans)
    
    #write.csv(from.xy, paste('./out.files/fromxy.', bin, '.csv', sep=''), row.names=F)
    write.csv(from.xy, paste("E:/Don_Gen_connectivity/out.files/fromxy", bin, '.csv', sep=''), row.names=F)
    #write.csv(to.xy, paste('./out.files/toxy.', bin, '.csv', sep=''), row.names=F)
    write.csv(to.xy, paste("E:/Don_Gen_connectivity/out.files/toxy", bin, '.csv', sep=''), row.names=F)
    #saveRDS(trans,paste('./out.files/trans.', bin,'.rds', sep=''))
    saveRDS(trans,paste("E:/Don_Gen_connectivity/out.files/trans", bin,'.rds', sep=''))
  }	
}

#Run loop to execute main function
ncore <- 12  ## choose the number of cores for parallel processing
cl <- makeCluster(ncore)
registerDoParallel(cl)

clusterExport(cl,c('bin.df','xy','pc1.stack','pc2.stack', 'cp', 'resistance.mask','x.min', 'x.max', 'y.min', 'y.max'))#; Export the environment variables to each cluster

clusterEvalQ(cl, library(gdistance)) 
clusterEvalQ(cl, library(raster))  
clusterEvalQ(cl, library(rgdal))  
clusterApplyLB(cl, x=1:nrow(bin.df), fun=main.function) 
stopCluster(cl)
##End of production of source and target points and transition layer for each climate type


##Part 3. Generation of current-flow centrality
passage.function <- function(bin) {
  if(file.exists(paste("E:/Don_Gen_connectivity/out.files/trans", bin,'.rds', sep=''))){
    trans<-readRDS(paste("E:/Don_Gen_connectivity/out.files/trans", bin,'.rds', sep=''))
    from.xy<-read.csv(paste("E:/Don_Gen_connectivity/out.files/fromxy", bin, '.csv', sep=''))
    to.xy<-read.csv(paste("E:/Don_Gen_connectivity/out.files/toxy", bin, '.csv', sep=''))
    pass<-tryCatch({passage(trans,as.matrix(as.data.frame(lapply(from.xy[,1:2], as.numeric))),as.matrix(as.data.frame(lapply(to.xy[,1:2], as.numeric))),theta=0,output="RasterLayer")}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})	
    pass<-tryCatch({extend(pass,t1extent)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

ncore <- 28  ## choose the number of cores for parallel processing
cl <- makeCluster(ncore)
registerDoParallel(cl)
clusterExport(cl,c('bin.df','t1extent'))	
clusterEvalQ(cl, library(gdistance)) 
clusterEvalQ(cl, library(raster)) 
clusterEvalQ(cl, library(rgdal)) 
out.passage.rasters<-clusterApplyLB(cl, x=1:nrow(bin.df), fun=passage.function) 
out.passage.rasters<-Filter(Negate(is.null), out.passage.rasters)
stack1<-stack((out.passage.rasters))
stopCluster(cl)
passmeanall<-calc(stack1,fun=mean,na.rm=TRUE)
passsumall<-calc(stack1,fun=sum,na.rm=TRUE)
writeRaster(passsumall,file="E:/Don_Gen_connectivity/passsumall.tif",overwrite=TRUE)
writeRaster(passmeanall,file="E:/Don_Gen_connectivity/passmeanall.tif",overwrite=TRUE)

##Part 4. Generation of shortest-path centrality
bigextent<-extent(mask)#for 'mask', specify a raster encompassing your analysis region
tmp.raster <- raster(ext=bigextent, resolution=c(5000,5000),crs="+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")#modify resolution and crs as needed
tmp.raster[tmp.raster == 1] <- 0

shortestpath.function <- function(bin) {
  if(file.exists(paste("E:/Don_Gen_connectivity/out.files/trans", bin,'.rds', sep=''))){
    trans<-readRDS(paste("E:/Don_Gen_connectivity/out.files/trans", bin,'.rds', sep=''))
    from.xy<-read.csv(paste("E:/Don_Gen_connectivity/out.files/fromxy",bin, '.csv', sep=''))#this generates forward shortest paths
    to.xy<-read.csv(paste("E:/Don_Gen_connectivity/out.files/toxy", bin, '.csv', sep=''))#to generate backward shortest paths, swap fromxy.csv and toxy.csv inputs	
    
    sl <- SpatialLines(LinesList = list(Lines(Line(matrix(0, ncol = 2)), ID = NA)))
    paths.template <- sl[0]
    
    cost.distance <- round(costDistance(trans, as.matrix(from.xy[,c('x','y')]), as.matrix(to.xy[c('x','y')])))
    cost.distance1 <- as.data.frame(cost.distance) 
    rm(cost.distance)
    gc()
    
    out.lcd <- as.data.frame(apply(cost.distance1, 1, min)) ## select the 'least-cost' of all climate analogs
    names(out.lcd) <- 'cost.dist'
    
    from.x <- from.xy[,1]
    from.y <- from.xy[,2]
    
    ## Set up output data frame. Other data will be added in future steps
    out.df <- as.data.frame(cbind(from.x, from.y))
    out.df$to.x <- 0; out.df$to.y <- 0; out.df$MED <- 0
    
    ##########################################################################################
    ## End: calculate least accumulated cost to all pixels identified as a climate analog   ##
    ##########################################################################################
    
    ################################################################################
    # Identify location (x and y coordinate) of pixel with least accumulated cost ##
    ################################################################################
    
    the.function <- function(k) {	
      from.index <- which(out.lcd[k,] == cost.distance1[k,])		
      # if there is more than one "to" with matching minumum distances, choose the first in the list
      if (length(from.index) > 1) {
        from.index[1]
        ID <- out.df[k, 'ID']
        to.x <- to.xy[from.index[1],][1]
        to.y <- to.xy[from.index[1],][2]
        return(c(to.x, to.y))
      } else {
        to.x <- to.xy[from.index,][1]
        to.y <- to.xy[from.index,][2]
        return(c(to.x, to.y))
      }
    }
    
    out <- (sapply(X=1:nrow(out.df), the.function))
    
    ## add coordinates for climate analog with the least accumulated cost
    out.df$to.x <- as.numeric(out[1,])
    out.df$to.y <- as.numeric(out[2,])
    
    #####################################################################################
    # End: Identify location (x and y coordinate) of pixel with least accumulated cost ##
    #####################################################################################
    
    ##############################################################################################
    ## Delineate the trajectory, or path, between each pixel and its 'nearest' climate analog   ##
    ##############################################################################################
    
    # make new dataframe for this in cases the source and destination locations are the same.
    # i.e. climate does not change
    sp.final <-paths.template
    out.df.final <- as.data.frame(matrix(nrow=0, ncol=7))
    colnames(out.df.final) <- c("from.x","from.y","to.x","to.y","MED","vel_MED","MCE")
    
    no.change.df.final <- as.data.frame(matrix(nrow=0, ncol=5))
    colnames(no.change.df.final) <- c("from.x","from.y","to.x","to.y","MED") 
    
    no.analog.df.final <- as.data.frame(matrix(nrow=0, ncol=2))
    colnames(no.analog.df.final) <- c("x","y") 
    
    out.df.new <- out.df[out.df$from.x != out.df$to.x | out.df$from.y != out.df$to.y,]
    no.change.df <- out.df[out.df$from.x == out.df$to.x & out.df$from.y == out.df$to.y,]
    
    if (nrow(out.df.new) > 0) {
      the.shortest.path.function <- function(k) {
        shortest.path <- shortestPath(trans, c(out.df.new[k, 'from.x'], out.df.new[k, 'from.y']), c(out.df.new[k, 'to.x'], out.df.new[k, 'to.y']), output="SpatialLines")
        return(shortest.path)					
      }
      
      sp <- sapply(X=1:nrow(out.df.new), FUN=the.shortest.path.function)	
      
      sp.final <- append(sp.final, sp)
    }
    
    ## This function is necessary to 'merge' all trajectories into one SpatialLines object per climate type
    makeUniqueIDs <- function(lst) {
      ids = sapply(lst, function(i) slot(i, "ID"))
      if (any(duplicated(ids))) {
        ids <- make.unique(as.character(unlist(ids)), sep = "")
        for (i in seq(along = ids))
          lst[[i]]@ID = ids[i]
      }
      lst
    }
    
    ll = do.call("c", lapply(sp.final, function(x) slot(x, "lines")))
    ll = makeUniqueIDs(ll)
    sp.final <- SpatialLines(ll, proj4string = CRS(proj4string(sp[[1]])))	
    
    ###################################################################################################
    ## End: Delineate the trajectory, or path, between each pixel and its 'nearest' climate analog   ##
    ###################################################################################################
    
    #####################
    ## Final touches   ##
    #####################
    
    out.df.final <- rbind(out.df.final, out.df.new)
    no.change.df.final <- rbind(no.change.df.final, no.change.df)
    nrow(out.df.final); length(sp.final)
    
    ## Calculate minimum exposure distance (MED) and assign to resulting data frame
    out.df.final$MED <- round((SpatialLinesLengths(sp.final) / 1000), 1) # convert to km
    
    ## Calculate velocity based on MED (equation 2 in Dobrowski and Parks [2016])
    out.df.final$vel_MED <- round(out.df.final$MED/90, 2) # 90 is number of years from 1995 to 2085
    
    out.df.final$CE.tmp <- ((out.lcd$cost.dist/1000)) / 2
    
    write.csv(out.df.final, paste("E:/Don_Gen_connectivity/output/data", bin,".cp",cp, '.csv', sep=''), row.names=F)
    
    ## Make Spatial lines dataframe ## Save trajectories as shapefile
    the.pd <- "E:/Don_Gen_connectivity/out.files/paths" # this might be wrong
    
    shortest.paths <- SpatialLinesDataFrame(sl=sp.final, data=out.df.final, match.ID=F)
    layer.name <- paste0('paths.', bin)
    writeOGR(obj=shortest.paths, layer=layer.name, dsn=the.pd, driver='ESRI Shapefile', overwrite=T)
    
    sp.buffer <- buffer(sp.final, 2500, dissolve=F)
    sp.buffer.sf<-st_as_sf(sp.buffer)
    line.count.raster <- fasterize(sp.buffer.sf, tmp.raster, fun='count')
    
    writeRaster(line.count.raster,file=paste("E:/Don_Gen_connectivity/output/data", bin, '.tif', sep=''),format="GTiff", overwrite=TRUE,options=c("COMPRESS=LZW"))
    
  }
}

ncore <- 28  ## choose the number of cores for parallel processing
cl <- makeCluster(ncore)
registerDoParallel(cl)

clusterExport(cl,c('cp','tmp.raster'))# Export the environment variables to each cluster

clusterEvalQ(cl, library(gdistance)) 
clusterEvalQ(cl, library(raster))  
clusterEvalQ(cl, library(rgdal))  
clusterEvalQ(cl, library(terra))  
clusterEvalQ(cl, library(rgeos))
clusterEvalQ(cl, library(fasterize))
clusterEvalQ(cl, library(sf))

clusterApplyLB(cl, x=1:12600, fun=shortestpath.function) 
stopCluster(cl)

#Merge the rasters from all climate types into a composite output raster representing total count of paths in each cell across all climate types.

tif.function <- function(bin) {
  if(file.exists(paste("E:/Don_Gen_connectivity/output/data", bin,'.tif', sep=''))){
    pass<-raster(paste("E:/Don_Gen_connectivity/output/output/data", bin,'.tif', sep=''))
    pass<-extend(pass,bigextent)
    pass<-crop(pass,bigextent)
  }
}	

ncore <- 28  ## number of cores for parallel processing
cl <- makeCluster(ncore)
registerDoParallel(cl)

clusterExport(cl,c('bigextent'))

clusterEvalQ(cl, library(gdistance)) ## Load the library "gdistance" to each cluster
clusterEvalQ(cl, library(raster)) ## Load the library "gdistance" to each cluster
clusterEvalQ(cl, library(rgdal)) ## Load the library "gdistance" to each cluster

tifstack<-clusterApplyLB(cl, x=1:12600, fun=tif.function)
tifstack<-Filter(Negate(is.null), tifstack)
stacktif<-stack(tifstack)
stopCluster(cl)
tifstacksum<-calc(stacktif,fun=sum,na.rm=T)
writeRaster(tifstacksum,file="E:/Don_Gen_connectivity/tifstacksum.tif",format="GTiff", overwrite=TRUE,options=c("COMPRESS=LZW"))
rm(tifstack)
rm(stacktif)
gc()

#Merge shortest-path output from all climate types into composite output spatial lines object.


the.prj <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
files <- list.files(pattern='.shp')
final.line.data <- ''

merge.function <- function (i) {
  line.data <- st_read(files[i])
  return(line.data)
}

ncore <- 25  ## choose the number of cores for parallel processing
cl <- makeCluster(ncore)
registerDoParallel(cl)
clusterExport(cl,c('files'))
clusterEvalQ(cl, library(sf)) 
FINAL<-clusterApplyLB(cl, x=1:length(files), fun=merge.function) ##this is quick enough that the parallelization can be avoided if desired
FINAL <- do.call(rbind, FINAL) ## this takes a while
stopCluster(cl)
st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84")$proj4string

## Save merged trajectories as shapefile
layer.name <- 'paths.all.shp'
st_write(obj=FINAL, dsn=layer.name, delete_dsn=TRUE)

## Save merged trajectories as raster
final.st<-as(FINAL,'Spatial')
final.buffer <- buffer(final.st, 100, dissolve=F)
final.buffer.sf<-st_as_sf(final.buffer)
linecountgrd <- fasterize(final.buffer.sf, tmp.raster, fun='count' )
writeRaster(shortestpathgrd,file="E:/Don_Gen_connectivity/shortestpathgrd.tif")

##End
