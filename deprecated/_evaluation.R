####################################################################################################################
####################################################################################################################
####################################################################################################################
############################################################    SDM USING SDM PACKAGE    ###########################
###########################################################       CHRYSTIAN C. SOSA         ########################
###########################################################             2017                       #################
####################################################################################################################
####################################################################################################################
####################################################################################################################
require(raster)


evaluation_function<-function(m2_eval,mInfo,nAUC,p2m,p2m_all,current_Out_Dir,cAUC_INC){

#####LOADING DATA AND CREATING MATRIX TO RUN THE EVALUATION#####

cat("#############################################","\n")
cat("STARTING EVALUATION METRICS STEP, BE PATIENT!","\n")
cat("#############################################","\n")

current_Out_Dir<-current_Out_Dir
cAUC_INC<-cAUC_INC
evaluation<-as.data.frame(matrix(nrow =length(unique(mInfo$method)),ncol=ncol(m2_eval)))
colnames(evaluation)<-colnames(m2_eval)
colnames(evaluation)[1]<-"METHOD"
evaluation$METHOD<-unique(mInfo$method)

evaluation$AUC_SD<-NA
evaluation$AUC_CV<-NA
evaluation$COR_CV<-NA
evaluation$Deviance_CV<-NA
evaluation$threshold_CV<-NA
evaluation$sensitivity_CV<-NA
evaluation$specificity_CV<-NA
evaluation$TSS_CV<-NA
evaluation$Kappa_CV<-NA
evaluation$NMI_CV<-NA
evaluation$ppv_CV<-NA
evaluation$npv_CV<-NA
evaluation$ccr_CV<-NA



evaluation$szCpt<-NA
evaluation$szCptUncertain<-NA
evaluation$rateCpt<-NA
evaluation$szThr<-NA
evaluation$szThrUncertain<-NA
evaluation$rateThr<-NA
evaluation$nAUC<-nAUC
evaluation$cAUC<-NA
evaluation$valid_model<-NA




pred_mean_names<-names(p2m)
preds_names<-names(p2m_all)

#####CALLING EVALUATION METRICS#####

cat("Calculating Medians and CV for SDMs metrics","\n")

for(i in 1:length(unique(mInfo$method))){
  method_sdm<-as.character(unique(mInfo$method)[i])
  X<-subset(m2_eval,mInfo$method==method_sdm)
  evaluation$AUC[i]<-median(X$AUC)
  evaluation$COR[i]<-median(X$COR)
  evaluation$Deviance[i]<-median(X$Deviance)
  evaluation$Prevalence[i]<-median(X$Prevalence)
  evaluation$threshold[i]<- median(X$threshold)
  evaluation$sensitivity[i]<-median(X$sensitivity)
  evaluation$specificity[i]<-median(X$specificity)
  evaluation$TSS[i]<-median(X$TSS)
  evaluation$Kappa[i]<-median(X$Kappa)
  evaluation$NMI[i]<-median(X$NMI)
  evaluation$phi[i]<-median(X$phi)
  evaluation$ppv[i]<-median(X$ppv)
  evaluation$npv[i]<-median(X$npv)
  evaluation$ccr[i]<-median(X$ccr)
  evaluation$prevalence[i]<-median(X$prevalence) 
  #####
  evaluation$AUC_SD[i]<-sd(X$AUC)
  evaluation$AUC_CV[i]<-cv(X$AUC)
  evaluation$COR_CV[i]<-cv(X$COR)
  evaluation$Deviance_CV[i]<-cv(X$Deviance)
  evaluation$threshold_CV[i]<-cv(X$threshold)
  evaluation$sensitivity_CV[i]<-cv(X$sensitivity)
  evaluation$specificity_CV[i]<-cv(X$specificity)
  evaluation$TSS_CV[i]<-cv(X$TSS)
  evaluation$Kappa_CV[i]<-cv(X$Kappa)
  evaluation$NMI_CV[i]<-cv(X$NMI)
  evaluation$ppv_CV[i]<-cv(X$ppv)
  evaluation$npv_CV[i]<-cv(X$npv)
  evaluation$ccr_CV[i]<-cv(X$ccr)
    
};rm(i)


sd1_list<-list()

#####CALCULATING ASD15#####
cat("Calculating ASD15","\n")
for(i in 1:length(unique(mInfo$method))){
  
  method<-unique(mInfo$method)[i]
  fun <- function(x) { sd(x,na.rm=T) }
  sd1<-calc(x = p2m_all[[grep(method,preds_names)]],fun = fun);gc()
  sd1_list[[i]]<-sd1
  names(sd1_list[[i]])<-as.character(method)
  esdCpt<-sd1
  esdThr<-esdCpt
  dumm<-raster(p2m, layer = grep(method,names(p2m)))
  esdCpt[which(dumm[] < 0.001)] <- NA
  esdThr[which(esdThr[] == 0)] <- NA
  cat("...Calculating ASD15 for: ",as.character(method),"\n")
  szCpt <- length(which(esdCpt[] >= 0))
  szCptUncertain <- length(which(esdCpt[] >= 0.15))
  rateCpt <- szCptUncertain / szCpt * 100
  
  szThr <- length(which(esdThr[] >= 0))
  szThrUncertain <- length(which(esdThr[] >= 0.15))
  rateThr <- szThrUncertain / szThr * 100##important
  if (is.na(rateThr)) {rateThr <- 100}
  evaluation$szCpt[i]<-szCpt
  evaluation$szCptUncertain[i]<-szCptUncertain
  evaluation$rateCpt[i]<-rateCpt
  evaluation$szThr[i]<-szThr
  evaluation$szThrUncertain[i]<-szThrUncertain
  evaluation$rateThr[i]<-rateThr
  
};rm(i)
#########################
cat("Writing SD rasters","\n")
sd1_list<-stack(sd1_list);gc()
writeRaster(sd1_list,paste0(current_Out_Dir,"/","SD_MODELS.img"));gc()
#####CALCULATING cAUC#####
cat("Calculating cAUC","\n")


for(i in 1:length(unique(mInfo$method))){
  
  calc_cauc <- function(i){
    AUC  <- evaluation$AUC[i]
    nAUC <- nAUC
    cAUC <- AUC+.5-max(c(.5,nAUC,na.rm=T))
    return(cAUC)
  }
  
  evaluation$cAUC[i]<-calc_cauc(i)
  
};rm(i)
#####CALCULATING VALID MODELS#####

cat("Calculating valid models","\n")

for(i in 1:length(unique(mInfo$method))){

#cat("Calculating valid models","\n")
  if(cAUC_INC==T){ 
    cat("Using cAUC approach","\n")
if(evaluation$AUC[i]>=0.7 & 
     evaluation$AUC_SD[i]<=0.15& 
     evaluation$TSS[i]>=0.4 & 
     evaluation$rateThr[i]<=10 &
     evaluation$cAUC[i]>=0.4 ){

evaluation$valid_model[i]<-1
      }else{
  evaluation$valid_model[i]<-0  
      }
    
}else{
  cat("Ommiting cAUC approach","\n")
  
  if(evaluation$AUC[i]>=0.7 & 
       evaluation$AUC_SD[i]<=0.15& 
       evaluation$TSS[i]>=0.4 & 
       evaluation$rateThr[i]<=10 ){
    
    evaluation$valid_model[i]<-1
  }else{
    evaluation$valid_model[i]<-0  
  }
  
      }
};rm(i)
cat("################","\n")
cat("EVALUATION DONE!","\n")
cat("################","\n")

return(evaluation)

}


