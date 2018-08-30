require(raster)



quantile_ensemble <- function(x, quantile)
{
  tot_ensemble <- sum(x,na.rm=T)
  tot_ensemble[which(tot_ensemble[] < nlayers(x)*quantile)] <- NA
  tot_ensemble[which(!is.na(tot_ensemble[]))] <- 1
  return(tot_ensemble)
}

eval_to_ens<-function(models_to_ensemble,m2_eval,sp_Dir,mInfo,m2_rep,p2m_all){
  cat("#############################################","\n")
  cat("STARTING ENSAMBLE APPROACH  STEP, BE PATIENT!","\n")
  cat("#############################################","\n")
  cat("USING: ",as.character(sp_Dir),"\n")
  
  cat("#############################################","\n")
  
  #m2_rep<-m2_rep
 #sp_Dir<-sp_Dir
cat("#############################################","\n")
cat("EXTRACTING REPLICATES WITH AUC>=0.7 |TSS>=0.4 AND CORRECT CLASSIFIED RATE >=70%","\n")

models_to_ensemble_MOD<-as.character(evaluation$METHOD[which(evaluation$valid_model==1)])

models_to_ensemble2<-unlist(lapply(1:length(models_to_ensemble_MOD),function(i){
  X<-subset(mInfo$modelID,(m2_rep$method==models_to_ensemble_MOD[[i]] &
                             mInfo$success==TRUE &
                             m2_rep$valid_model==1
  ))
  
  return(X)
})
)
cat("USING ",sp_Dir,"\n")
cat("#############################################","\n")
cat("THRESHOLDING REPLICATES WITH AUC>=0.7 |TSS>=0.4 AND CORRECT CLASSIFIED RATE >=70%","\n")
cat("                                             ","\n")

if(!file.exists(paste0(sp_Dir,"/","Threshold_all.img"))){
p2m_quantile2<-lapply(1:length(models_to_ensemble2),function(i){
  id<-models_to_ensemble2[[i]]
  x<-subset(m2_rep,m2_rep$modelID==id)
  
  cat("thresholding layer: ",i," of ",length(models_to_ensemble2),"| ModelID:",id,"\n")
  
  p_raster<-p2m_all[[id]]
  names(p_raster)<-paste0(x$method,"_",x$modelID)
    #names(p2m_all[[id]])
  p_raster[which(p_raster[]>=m2_eval$threshold[[id]])]<-1
  p_raster[which(p_raster[]<m2_eval$threshold[[id]])]<-0
  return(p_raster)
});gc()
cat("SAVING FILE in ",sp_Dir,"\n")
p2m_quantile2<-stack(p2m_quantile2)
writeRaster(p2m_quantile2,paste0(sp_Dir,"/","Threshold_all.img"),overwrite=F)
}else{
  cat("Threshold file available, using it","\n")
  p2m_quantile2<-stack(brick(paste0(sp_Dir,"/","Threshold_all.img")))
}
cat("#############################################","\n")
cat("ENSEMBLE PER VALID ALGORITHM","\n")

if(!file.exists(paste0(sp_Dir,"/","MCAA_per_algorithms.img"))){
p2m_ens_to_2<-lapply(1:length(models_to_ensemble_MOD),function(i){
  cat("Calculating Models committee averaging for ",
      as.character(models_to_ensemble_MOD[[i]]),
      ": |",
      i,
      
      " of ",
      length(models_to_ensemble_MOD),
      " Valid algorithms!",
      "\n")
  
  X<-grep(models_to_ensemble_MOD[[i]],names(p2m_quantile2))
  
  p<-sum(p2m_quantile2[[X]],na.rm=T)/length(X)
  
  names(p)<-as.character(paste0(models_to_ensemble_MOD[[i]]),"_","MCAA")
  cat(names(p)," DONE!","\n")
  return(p)
});gc()

p2m_ens_to_2<-stack(p2m_ens_to_2)
writeRaster(p2m_ens_to_2,paste0(sp_Dir,"/","MCAA_per_algorithms.img"))
}else{
  cat("MCAA per algorithm file available, using it","\n")
  p2m_ens_to_2<-stack(brick(paste0(sp_Dir,"/","MCAA_per_algorithms.img")))
  }

cat("#############################################","\n")
cat("QUANTILE ENSEMBLE PER VALID ALGORITHM","\n")


if(!file.exists(paste0(sp_Dir,"/","MCAA.img"))){
  
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
writeRaster(p2m_ens_to_quantile2,paste0(sp_Dir,"/","MCAA.img"))

}else{
  cat("MCAA  file available, using it","\n")
  p2m_ens_to_quantile2<-stack(brick(paste0(sp_Dir,"/","MCAA.img")))
              
}

cat("#############################################","\n")
cat("SAVING FINAL ENSEMBLE RASTER","\n")
#plot(p2m_ens_to_quantile2)
p2m_ens_to_quantile_final_mean2<-sum(p2m_quantile2,na.rm=T)/nlayers(p2m_quantile2)
names(p2m_ens_to_quantile_final_mean2)<-"Qu_mean_all_thr_raster"
writeRaster(p2m_ens_to_quantile_final_mean2,paste0(sp_Dir,"/","MCAA_FINAL",".tif"))
      
p2m_ens_to_quantile_final_mean2<-quantile_ensemble(p2m_ens_to_quantile_final_mean2,quantile=0.5);gc()


writeRaster(p2m_ens_to_quantile_final_mean2,paste0(sp_Dir,"/","MCAA_FINAL_THR",".tif"))

cat("                                             ","\n")
cat("#############################################","\n")
cat("DONE!","\n")
cat("#############################################","\n")
cat("                                             ","\n")

}

