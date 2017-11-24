####################################################################################################################
####################################################################################################################
####################################################################################################################
############################################################    SDM USING SDM PACKAGE    ###########################
###########################################################       CHRYSTIAN C. SOSA         ########################
###########################################################             2017                       #################
####################################################################################################################
####################################################################################################################
####################################################################################################################
require(dismo)


##########################################
#
#####BASELINE
#
##########################################

input_dir<-"V:/02_Gridded_data"
###BASELINE DIRECTORY
bl_dir<-paste0(input_dir,"/","baseline_2_5min_v2/average")
Out_dir<-paste0(bl_dir,"/","biovars");if(!file.exists(paste0(bl_dir,"/","biovars"))){dir.create(paste0(bl_dir,"/","biovars"))}



#TEMPERATURE MIN
tmin<-paste0("tmin_",1:12,".tif")
 tmin<-stack(lapply(1:length(tmin),function(i){
x<-raster(paste0(bl_dir,"/",tmin[[i]]))  
crs(x)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  return(x)
  cat((i/length(tmin*100))," %","\n")
  
}));gc()

#TEMPERATURE MAX
tmax<-paste0("tmax_",1:12,".tif")
tmax<-stack(lapply(1:length(tmax),function(i){
  x<-raster(paste0(bl_dir,"/",tmax[[i]]))  
  crs(x)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  return(x)
  cat((i/length(tmax*100))," %","\n")
  
}));gc()

#PREC
prec<-paste0("prec_",1:12,".tif")
prec<-stack(lapply(1:length(prec),function(i){
  x<-raster(paste0(bl_dir,"/",prec[[i]]))  
  crs(x)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  return(x)
  cat((i/length(prec*100))," %","\n")
  
}));gc()

x<-dismo::biovars(prec=prec, tmin=tmin, tmax=tmax);gc()

##SAVE BIOVARS

lapply(1:nlayers(x),function(i){

  writeRaster(x[[i]],paste0(Out_dir,"/",names(x[[i]]),".tif"))  
  
  
});gc()


##########################################
#
#####GCM
#
##########################################

BIOVARS_FUNCTION<-function(input_dir){
  
scenarios<-list.dirs(input_dir,recursive = F,F)
  
#input_dir<-"V:/03_Future_data/downscaling_bsl_2_5min_v1"
#scenarios<-list.dirs(input_dir,recursive = F,F)

lapply(1:length(scenarios),function(i){
  
  scenario_dir<-paste0(input_dir,"/",scenarios[[i]])
  gcms<-list.dirs(scenario_dir,recursive = F,F)
  
  
       lapply(1:length(gcms),function(j){
         
        gcm_dir<-paste0(input_dir,"/",scenarios[[i]],"/",gcms[[j]])
        periods<-list.dirs(gcm_dir,recursive = F,F)
      
      
                lapply(1:length(periods),function(k){
                  
                  
                  period_dir<-paste0(gcm_dir,"/",periods[[k]])
                  
                  
cat("###########################################","\n")
cat("#PROCESSING: ", as.character(scenarios[[i]])," | ",as.character(gcms[[j]])," | ",as.character(periods[[k]]),"\n")
cat("###########################################","\n")
###########################################################  

Out_dir<-paste0(period_dir,"/","biovars");if(!file.exists(paste0(period_dir,"/","biovars"))){dir.create(paste0(period_dir,"/","biovars"))}
                  
###########################################################  
                  
                  
                  
###########################################################    
#TEMPERATURE MIN
tmin<-paste0("tmin_",1:12,".tif")
tmin<-stack(lapply(1:length(tmin),function(l){
  x<-raster(paste0(period_dir,"/",tmin[[l]]))  
  crs(x)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  return(x)
  cat((l/length(tmin*100))," %","\n")
  
}));gc()

#TEMPERATURE MAX
tmax<-paste0("tmax_",1:12,".tif")
tmax<-stack(lapply(1:length(tmax),function(l){
  x<-raster(paste0(period_dir,"/",tmax[[l]]))  
  crs(x)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  return(x)
  cat((l/length(tmax*100))," %","\n")
  
}));gc()

#PREC
prec<-paste0("prec_",1:12,".tif")
prec<-stack(lapply(1:length(prec),function(l){
  x<-raster(paste0(period_dir,"/",prec[[l]]))  
  crs(x)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  return(x)
  cat((l/length(prec*100))," %","\n")
  
}));gc()
###########################################################  
cat("###########################################","\n")
cat("CALCULATING BIOCLIM VARS","\n")
cat("###########################################","\n")

x<-dismo::biovars(prec=prec, tmin=tmin, tmax=tmax);gc()
###########################################################  
##SAVE BIOVARS

lapply(1:nlayers(x),function(l){
  
  writeRaster(x[[l]],paste0(Out_dir,"/",names(x[[l]]),".tif"))  
  
  
});gc()

###########################################################  
cat("###########################################","\n")
cat("DONE!","\n")
cat("###########################################","\n")


                })
        })        
 })

}

input_dir<-"V:/03_Future_data/downscaling_bsl_2_5min_v1"


x<-BIOVARS_FUNCTION(input_dir)
   

