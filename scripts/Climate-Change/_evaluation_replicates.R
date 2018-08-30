
m2_rep_function<-function(mInfo){


  mInfo_id<-as.data.frame(cbind(as.numeric(mInfo$modelID),as.character(mInfo$method)))
  colnames(mInfo_id)<-c("modelID","method")
  mInfo_id$valid_algorithm<-NA
  #mInfo_id$valid_model<-1
  for(i in 1:length(models_to_ensemble_MOD)){
    mInfo_id$valid_algorithm[which(mInfo_id$method==models_to_ensemble_MOD[[i]])]<-1
    
  }
  
  
  models_to_ensemble<-unlist(lapply(1:length(models_to_ensemble_MOD),function(i){
    X<-subset(mInfo$modelID,(mInfo_id$method==models_to_ensemble_MOD[[i]] &
                               mInfo$success==TRUE &
                               mInfo_id$valid_algorithm==1
    ))  
    
    return(X)
  })
  )

m2_rep<-merge(as.data.frame(models_to_ensemble),m2_eval,by.x = "models_to_ensemble",by.y ="modelID")  
m2_rep<-merge(mInfo_id,m2_rep,by.x = "modelID",by.y ="models_to_ensemble")  

m2_rep$valid_model<-NA


for(i in 1:nrow(m2_rep)){
  
  #cat("Calculating valid models","\n")
  
  if(m2_rep$AUC[i]>=0.7 & 
       m2_rep$TSS[i]>=0.4 &
       m2_rep$ccr[i]>=0.7
  ){
    
    m2_rep$valid_model[i]<-1
  }else{
    m2_rep$valid_model[i]<-0  
  }
};rm(i)

write.csv(m2_rep,paste0(sp_Dir,"/","evaluation","/","Eval_replicates.csv"),quote=F,row.names=F)
return(m2_rep)
}