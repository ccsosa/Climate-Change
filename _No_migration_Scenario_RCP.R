require(raster)

No_migration_RCP_function<-function(mod_Dir,sp_name,sp_Dir_Original,RCP,PERIOD,models_to_ensemble_MOD,current_Out_Dir){

  ##########################################################
  ###CREATING PROJECTION FOLDERS 
  ######################################################## 
  ens_dir<-paste0(sp_Dir_Original,"/","ensemble")
  ##########################################################
  ###CREATING PROJECTION FOLDERS RCP FOLDER
  RCP_proj_dir<-paste0(ens_dir,"/",RCP)
  ##########################################################
  ###CREATING PROJECTION FOLDERS PERIOD FOLDER
  PERIOD_proj_dir<-paste0(RCP_proj_dir,"/",PERIOD)
  
  if(!file.exists(paste0(PERIOD_proj_dir,"/","MCAA_FINAL_THR_NO_MIG.tif"))){
    

  bl_thr<-raster(paste0(current_Out_Dir,"/","MCAA_FINAL_THR.tif"))
 
  ###ENSEMBLE MCAA PER ALGORITH
  cat("Getting new MCAA per algorithm NO migration scenario","\n")
  
  p2m_ens_to_2<-stack(brick(paste0(PERIOD_proj_dir,"/","MCAA_ALGORTIHM.img")))
  names(p2m_ens_to_2)<-models_to_ensemble_MOD
  
  p2m_ens_to_2<-p2m_ens_to_2*bl_thr
  names(p2m_ens_to_2)<-models_to_ensemble_MOD
  writeRaster(p2m_ens_to_2,paste0(PERIOD_proj_dir,"/","MCAA_ALGORTIHM_NO_MIG.img"))
  

  cat("Getting new MCAA per algorithm NO migration scenario (QUANTILE APPROACH)","\n")
  p2m_ens_to_quantile2<-stack(brick(paste0(PERIOD_proj_dir,"/","MCAA_ALGORTIHM_THR.img")))
  names(p2m_ens_to_quantile2)<-models_to_ensemble_MOD
   p2m_ens_to_quantile2<-p2m_ens_to_quantile2*bl_thr
  names(p2m_ens_to_quantile2)<-models_to_ensemble_MOD
  
  writeRaster(p2m_ens_to_quantile2,paste0(PERIOD_proj_dir,"/","MCAA_ALGORTIHM_THR_NO_MIG.img"))
  
  ##########################################################
  ##########################################################
  ##########################################################
  ##########################################################
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
}