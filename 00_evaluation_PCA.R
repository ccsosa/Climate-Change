require(FactoMineR);require(ggplot2);library(ggfortify);require(scales)

PCA_EVAL_FUNCTION<-function(mInfo,m2_eval){
  
  mInfo_id<-as.data.frame(cbind(as.numeric(mInfo$modelID),as.character(mInfo$method)))
  colnames(mInfo_id)<-c("modelID","method")
  mEval_PCA<-merge(mInfo_id,m2_eval,by="modelID")
  mEval_PCA<-mEval_PCA[,-6]
  mEval_PCA<-mEval_PCA[,-1]
    
 
pc_aut<- autoplot(prcomp(mEval_PCA[,-1],scale=T),data=mEval_PCA,colour = "method",
          loadings = F, loadings.colour = 'black',
          loadings.label = TRUE, loadings.label.size = 5,
          frame = TRUE, 
          frame.type = 'norm',
          frame.colour = "method"
          )

ggsave(paste0(sp_Dir,"/","evaluation","/","PCA_EVAL","_",Sys.Date(),".pdf"),
 pc_aut,units="in",width=12.5,height=7.5,scale=2,dpi=600)

  
}

x<-PCA_EVAL_FUNCTION(mInfo,m2_eval)