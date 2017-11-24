
library(raster)
library(rgdal)


##########################################################
##########################################################
quantile_ensemble <- function(x, quantile)
{
  tot_ensemble <- sum(x,na.rm=T)
  tot_ensemble[which(tot_ensemble[] < nlayers(x)*quantile)] <- NA
  tot_ensemble[which(!is.na(tot_ensemble[]))] <- 1
  return(tot_ensemble)
}

##########################################################
##########################################################

ensemble_RCP_function<-function(mod_Dir,sp_name,sp_Dir_Original,RCP,PERIOD,GCMS){

  ##########################################################
  ##PREVIOUS STEPS##
  ##########################################################
  
sp_Dir<-paste0(mod_Dir,"/",sp_name);if(!file.exists(sp_Dir)){dir.create(sp_Dir)}
sp_Dir_Original<-sp_Dir
Eval_sp_Dir<-paste0(sp_Dir,"/","evaluation");if(!file.exists(Eval_sp_Dir)){dir.create(Eval_sp_Dir)}
rm(sp_Dir)
out_dir<-paste0(sp_Dir_Original,"/","ensemble");if(!file.exists(out_dir)){dir.create(out_dir)}

####################################################################################################################
##########################################################
##DEFINING FOLDER TO SAVE ARCHIVES
##########################################################

RCP_out_dir<-paste0(out_dir,"/",RCP);if(!file.exists(RCP_out_dir)){dir.create(RCP_out_dir)}
PERIOD_out_dir<-paste0(RCP_out_dir,"/",PERIOD);if(!file.exists(PERIOD_out_dir)){dir.create(PERIOD_out_dir)}
####################################################################################################################
if(!file.exists(paste0(PERIOD_out_dir,"/","MCAA_FINAL_THR.tif"))){

##########################################################
##CALLING PROJECTION FILES TO BE USED!
##########################################################
####################################################################################################################
###PROJECTING DIR 
proj_dir<-paste0(sp_Dir_Original,"/","projection");if(!file.exists(proj_dir)){dir.create(proj_dir)}

##########################################################
###CREATING ENSEMBLING FOLDERS 
######################################################## 
input_dir<-paste0(sp_Dir_Original,"/","projection")

##########################################################
###CREATING PROJECTION FOLDERS RCP FOLDER
RCP_input_dir<-paste0(proj_dir,"/",RCP);if(!file.exists(RCP_input_dir)){dir.create(RCP_input_dir)}
###CREATING PROJECTION FOLDERS PERIOD FOLDER
PERIOD_input_dir<-paste0(RCP_input_dir,"/",PERIOD);if(!file.exists(PERIOD_input_dir)){dir.create(PERIOD_input_dir)}

####################################################################################################################
####################################################################################################################


##########################################################
###CREATING PROJECTION FOLDERS 
######################################################## 
proj_dir<-paste0(sp_Dir_Original,"/","projection");if(!file.exists(proj_dir)){dir.create(proj_dir)}
##########################################################
###CREATING RCP FOLDER
RCP_proj_dir<-paste0(proj_dir,"/",RCP);if(!file.exists(RCP_proj_dir)){dir.create(RCP_proj_dir)}
##########################################################
###CALLING MCAA RASTERS

PERIOD_LAYERS<-paste0(RCP_proj_dir,"/",GCMS,"/",PERIOD,"/","MCAA_FINAL_THR.tif")
PERIOD_LAYERS<-lapply(1:length(PERIOD_LAYERS),function(i){
  
  x<-raster(PERIOD_LAYERS[[i]])
  return(x)
})

PERIOD_LAYERS<-stack(PERIOD_LAYERS)

####################################################################################################################
###CALCULATINGSD, MCAA and QUANTILE MCAA FINALS


cat("Calculating SD","\n")
fun <- function(x) { sd(x,na.rm=T) }
sd1<-calc(x = PERIOD_LAYERS,fun = fun);gc()
writeRaster(sd1,paste0(PERIOD_out_dir,"/","SD_MCAA",".tif"))

cat("Calculating MCAA raster","\n")
p<-sum(PERIOD_LAYERS,na.rm=T)/nlayers(PERIOD_LAYERS)
PERIOD_out_dir
writeRaster(p,paste0(PERIOD_out_dir,"/","MCAA_FINAL",".tif"))
cat("Calculating MCAA thresholded raster","\n")
p2<-quantile_ensemble(p,quantile=0.5);gc()
writeRaster(p2,paste0(PERIOD_out_dir,"/","MCAA_FINAL_THR",".tif"))

rm(p,p2,PERIOD_LAYERS)
####################################################################################################################
####################################################################################################################
mod_Dir<-paste0(mod_Dir);if(!file.exists(mod_Dir)){dir.create(mod_Dir)}

sp_Dir<-paste0(mod_Dir,"/",sp_name);if(!file.exists(sp_Dir)){dir.create(sp_Dir)}
sp_Dir_Original<-sp_Dir
Eval_sp_Dir<-paste0(sp_Dir,"/","evaluation");if(!file.exists(Eval_sp_Dir)){dir.create(Eval_sp_Dir)}

evaluation<-read.csv(paste0(Eval_sp_Dir,"/","metrics.csv"),header=T)

cat("Calling valid models!","\n")

valid_models<-as.character(unique(evaluation$METHOD[which(evaluation$valid_model==1)]))
PERIOD_LAYERS<-paste0(RCP_proj_dir,"/",GCMS,"/",PERIOD,"/","MCAA_per_algorithms.img")
PERIOD_LAYERS<-lapply(1:length(PERIOD_LAYERS),function(i){
  gc_val<-GCMS[[i]]
  x<-stack(brick(PERIOD_LAYERS[[i]]))
  names(x)<-(paste0(gc_val,"_",valid_models))
  return(x)
});gc()

PERIOD_LAYERS<-stack(PERIOD_LAYERS)


cat("Calculating MCAA for the RCP ","\n")


MCAA_PER_ALG<-lapply(1:length(valid_models),function(i){
X<-grep(valid_models[[i]],names(PERIOD_LAYERS))
X_alg<-sum(PERIOD_LAYERS[[X]],na.rm=T)/nlayers(PERIOD_LAYERS[[X]])
names(X_alg)<-paste0(valid_models[[i]],"_","MCAA")
return(X_alg)
})

MCAA_PER_ALG<-stack(MCAA_PER_ALG)
writeRaster(MCAA_PER_ALG,paste0(PERIOD_out_dir,"/","MCAA_ALGORTIHM",".img"))

cat("Calculating MCAA thresholded  for the RCP ","\n")


MCAA_PER_ALG_THR<-lapply(1:length(valid_models),function(i){
  X<-grep(valid_models[[i]],names(PERIOD_LAYERS))
  X_alg_thr<-quantile_ensemble(MCAA_PER_ALG[[i]],quantile=0.5);gc()
  names(X_alg_thr)<-paste0(valid_models[[i]],"_","MCAA_THR")
  return(X_alg_thr)
})

MCAA_PER_ALG_THR<-stack(MCAA_PER_ALG_THR)
writeRaster(MCAA_PER_ALG_THR,paste0(PERIOD_out_dir,"/","MCAA_ALGORTIHM_THR",".img"))

cat("                   ","\n")
cat("###################","\n")
cat("                   ","\n")
cat("DONE!              ","\n")
cat("                   ","\n")
cat("###################","\n")
cat("                   ","\n")
}else{
  
  cat("                   ","\n")
  cat("###################","\n")
  cat("                   ","\n")
  cat("ALREADY ENSEMBLED! ","\n")
  cat("                   ","\n")
  cat("###################","\n")
  cat("                   ","\n")
  
}
}