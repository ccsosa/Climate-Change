####################################################################################################################
####################################################################################################################
####################################################################################################################
############################################################    SDM USING SDM PACKAGE    ###########################
###########################################################       CHRYSTIAN C. SOSA         ########################
###########################################################             2017                       #################
####################################################################################################################
####################################################################################################################
####################################################################################################################


require(raster);require(rgdal);require(sp);require(dismo)




swd_function<-function(inDir,current_clim_dir,occ_Dir,bg_create){

current_clim_layer<-lapply(paste0(current_clim_dir,"/","/",paste0("bio_",1:19,".tif")),raster)
current_clim_layer<-stack(current_clim_layer)

e<-extent(current_clim_layer[[1]])

occs<-list.files(paste0(inDir,"/","occurrences"),".csv")



lapply(1:length(occs),function(i){
  
  
occFile<-paste0(paste0(inDir,"/","occurrences"),"/",occs[[i]])

    cat("Processing: ",as.character(occs[[i]]),"\n")
    spData <- read.csv(occFile)
    
    spData<-spData[which(!is.na(spData$lon)),]
    spData$status<-NA;spData$status<-1
    coordinates(spData)<-~lon+lat
    crs(spData)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
cat("Converting extent to polygon: ",as.character(occs[[i]]),"\n")

    p <- as(e, 'SpatialPolygons')
    crs(p) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    p$NAME<-1
    ovr<-over(spData,p)
    spData<-spData[!is.na(ovr$NAME),]
    
    xval<-extract(current_clim_layer,spData)
    spData<-cbind(spData,xval)
    spData2<-as.data.frame(spData)
    spData2<-spData2[,c( "id","species","source","status","lon","lat",paste0("bio_",1:19))]
    



if(bg_create==T){
  
  cat("BACKGROUND TO DO IT, CREATING THEM","\n")
  
    xran<-randomPoints(mask=current_clim_layer[[1]],n=10000,p=spData,excludep=T,lonlatCorrection=T)
    ex_raster_env<-extract(current_clim_layer,xran)
    count2<-1:nrow(xran)
    
    sp_name<-rep(spData$species[[1]],nrow(xran))
    source_d<-rep("bg",nrow(xran))
    status<-rep(0,nrow(xran))
    
    z = as.data.frame(cbind(count2,as.character(sp_name),source_d,status,xran,ex_raster_env))
    colnames(z)<-c( "id","species","source","status","lon","lat",paste0("bio_",1:19))
    joinS<-rbind(spData2,z)
    #colnames(z)<-c("taxon","lon","lat",paste0("bio_", 1:19))
    joinS<-joinS[which(complete.cases(joinS)),]
  cat("Saving swd file for: ",as.character(occs[[i]]),"\n")
  
    outBackName<-paste0(bacK_Dir,"/",occs[[i]])
    out <- write.csv(joinS,outBackName, quote=F, row.names=F)
}else{
  
  spData2<-spData2[which(complete.cases(spData2)),]
  
  cat("NOT BACKGROUND TO DO IT, USING PRESENCES","\n")
  outBackName<-paste0(bacK_Dir,"/",occs[[i]])
  
  cat("Saving swd file for: ",as.character(occs[[i]]),"\n")
  out <- write.csv(spData2,outBackName, quote=F, row.names=F)
  
      }
    })
  }
