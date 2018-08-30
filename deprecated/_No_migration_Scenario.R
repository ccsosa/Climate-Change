require(raster)

quantile_ensemble <- function(x, quantile)
{
  tot_ensemble <- sum(x,na.rm=T)
  tot_ensemble[which(tot_ensemble[] < nlayers(x)*quantile)] <- NA
  tot_ensemble[which(!is.na(tot_ensemble[]))] <- 1
  return(tot_ensemble)
}

No_migration_Scenario_function<-function(mod_Dir,sp_name,sp_Dir_Original,RCP,PERIOD,GCM,models_to_ensemble_MOD,current_Out_Dir){
  
##########################################################
###CREATING PROJECTION FOLDERS 
######################################################## 
proj_dir<-paste0(sp_Dir_Original,"/","projection");if(!file.exists(proj_dir)){dir.create(proj_dir)}
##########################################################
###CREATING PROJECTION FOLDERS RCP FOLDER
RCP_proj_dir<-paste0(proj_dir,"/",RCP);if(!file.exists(RCP_proj_dir)){dir.create(RCP_proj_dir)}
##########################################################
###CREATING PROJECTION FOLDERS GCM FOLDER
GCM_proj_dir<-paste0(RCP_proj_dir,"/",GCM);if(!file.exists(GCM_proj_dir)){dir.create(GCM_proj_dir)}
##########################################################
###CREATING PROJECTION FOLDERS PERIOD FOLDER
PERIOD_proj_dir<-paste0(GCM_proj_dir,"/",PERIOD);if(!file.exists(PERIOD_proj_dir)){dir.create(PERIOD_proj_dir)}
cat(PERIOD_proj_dir,"\n")
if(!file.exists(paste0(PERIOD_proj_dir,"/","MCAA_FINAL_THR_NO_MIG.tif"))){
  
  #########################################################
  ##CALLING  CURRENT THRESHOLDED RASTER TO BE USED##########
  ##########################################################
  
  bl_thr<-raster(paste0(current_Out_Dir,"/","MCAA_FINAL_THR.tif"))
  
###ENSEMBLE MCAA PER ALGORITH
cat("Getting new MCAA per algorithm NO migration scenario","\n")

p2m_ens_to_2<-stack(brick(paste0(PERIOD_proj_dir,"/","MCAA_per_algorithms.img")))
names(p2m_ens_to_2)<-models_to_ensemble_MOD

p2m_ens_to_2<-p2m_ens_to_2*bl_thr
names(p2m_ens_to_2)<-models_to_ensemble_MOD
writeRaster(p2m_ens_to_2,paste0(PERIOD_proj_dir,"/","MCAA_per_algorithms_NO_MIG.img"))


########################################
##MCAA NO MIGRATION SCENARIO
##########################################################

cat("Getting new MCAA per algorithm NO migration scenario (QUANTILE APPROACH)","\n")

if(!file.exists(paste0(PERIOD_proj_dir,"/","MCAA.img"))){
  
  p2m_ens_to_quantile2<-lapply(1:nlayers(p2m_ens_to_2),function(i){
    cat("Calculating quantile threshold approach for ",
        as.character(models_to_ensemble_MOD[[i]]),
        ": |",
        i,
        
        " of ",
        length(models_to_ensemble_MOD),
        " Valid algorithms!",
        "\n")
    
   # X<-grep(models_to_ensemble_MOD[[i]],names(p2m_quantile2))
    
    p2<-quantile_ensemble(p2m_ens_to_2[[i]],quantile=0.5);gc()
    
    if(length(unique(p2[]))==2){
      
      names(p2)<-as.character(paste0(models_to_ensemble_MOD[[i]],"_QUANTILE"))
      cat(names(p2)," DONE!","\n")
    }else{
      
      
      cat(names(p2)," EMPTY!..SKIPPING","\n")
      p2<-NA
    }
    return(p2)
  });gc()
  
  cat("#############################################","\n")
  cat("ENSEMBLING VALID REPLICATES","\n")
  
  p2m_ens_to_quantile2<-p2m_ens_to_quantile2[!is.na(p2m_ens_to_quantile2)]
  
  p2m_ens_to_quantile2<-stack(p2m_ens_to_quantile2)
  writeRaster(p2m_ens_to_quantile2,paste0(PERIOD_proj_dir,"/","MCAA.img"))
  
}else{
  cat("MCAA  file available, using it","\n")
  p2m_ens_to_quantile2<-stack(brick(paste0(PERIOD_proj_dir,"/","MCAA.img")))
  
}

p2m_ens_to_quantile2<-p2m_ens_to_quantile2*bl_thr
names(p2m_ens_to_quantile2)<-models_to_ensemble_MOD

writeRaster(p2m_ens_to_quantile2,paste0(PERIOD_proj_dir,"/","MCAA_NO_MIG.img"))


#######################################
##MCAA NO MIGRATION FINAL SCENARIO
##########################################################
cat("Getting new MCAA final...NO migration scenario","\n")

MCAA_FINAL<-raster(paste0(PERIOD_proj_dir,"/","MCAA_FINAL.tif"))
MCAA_FINAL<-MCAA_FINAL*bl_thr
writeRaster(MCAA_FINAL,paste0(PERIOD_proj_dir,"/","MCAA_FINAL_NO_MIG.tif"))
####

cat("Getting new MCAA final...NO migration scenario (QUANTILE APPROACH)","\n")
MCAA_FINAL<-raster(paste0(PERIOD_proj_dir,"/","MCAA_FINAL_THR.tif"))
MCAA_FINAL<-MCAA_FINAL*bl_thr
writeRaster(MCAA_FINAL,paste0(PERIOD_proj_dir,"/","MCAA_FINAL_THR_NO_MIG.tif"))

  }else{
    
    cat("NO MIGRATION CODE WAS ALREADY RUN!, SKIPPING...","\n")
    
    
  }

cat("       ","\n")
cat("#######","\n")
cat("DONE!  ","\n")
cat("#######","\n")
cat("       ","\n")
}