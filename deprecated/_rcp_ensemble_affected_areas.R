require(raster)

RCP_ENSEMBLE_AFFECTED_AREAS<-function(sp_Dir_Original,RCPs,PERIODS,current_Out_Dir){
  
  
  bl_prob_thr<-raster(paste0(current_Out_Dir,"/","MCAA_FINAL_THR.tif"))
  
  bl_prob_thr_mask<-bl_prob_thr
  bl_prob_thr[which(is.na(bl_prob_thr[]))]<-0
  
  
  out_dir<-paste0(sp_Dir_Original,"/","ensemble")
  
  
areas_RCP_list<-lapply(1:length(RCPs),function(i){
  RCP<-RCPs[[i]]
  
  lapply(1:length(PERIODS),function(j){
  PERIOD<-PERIODS[[j]]
  
  RCP_out_dir<-paste0(out_dir,"/",RCP)
  PERIOD_out_dir<-paste0(RCP_out_dir,"/",PERIOD)
  
  x<-raster(paste0(PERIOD_out_dir,"/","MCAA_FINAL_THR.tif"))
  
  y<-raster::overlay(x,bl_prob_thr,fun=function(x, bl_prob_thr){
    x[which(x[]==1)]<-2
    x[which(is.na(x[]))]<-0
    y<-x-bl_prob_thr
    y[which(y[]==0)]<-NA
    return(y)})

  
  
  if(file.exists(paste0(PERIOD_out_dir,"/","Affected_areas.tif"))){
    cat("OMMITING...",paste0(PERIOD_out_dir,"/","Affected_areas.tif"),"\n")
  }else{
  writeRaster(y,paste0(PERIOD_out_dir,"/","Affected_areas.tif"))
  
  }
  yas<-as.data.frame(matrix(nrow=1,ncol=7))
  colnames(yas)<-c("RCP","PERIOD","RCP_SCENARIO","SCENARIO","NEW_AREAS","KEPT_AREAS","LOST_AREAS")
  yas[,1]<-RCP
  yas[,2]<-PERIOD
  yas[,3]<-paste0(RCP,"_",PERIOD)
  yas[,4]<-"MIGRATION"
  yas[,5]<-length(y[][which(y[]==2)])
  yas[,6]<-length(y[][which(y[]==1)])
  yas[,7]<-length(y[][which(y[]==-1)])
  
  return(yas)
  })
});gc()
  
  
areas_RCP_list2 <- unlist(areas_RCP_list, recursive = FALSE)
areas_RCP_list2<-do.call(rbind,areas_RCP_list2)

write.csv(areas_RCP_list2,paste0(out_dir,"/","AFFECTED_AREA_SCENARIO.csv"),row.names=F,quote=F)


areas_RCP_list2$LOST_AREAS<--(areas_RCP_list2$LOST_AREAS)
dat_RCP <- melt(areas_RCP_list2,id.vars=c("RCP_SCENARIO"), measure.vars=c("LOST_AREAS","KEPT_AREAS","NEW_AREAS"));gc()
dat_RCP$variable<-as.character(dat_RCP$variable)


dat_RCP$variable[which(dat_RCP$variable=="LOST_AREAS")]<-"Lost areas"
dat_RCP$variable[which(dat_RCP$variable=="KEPT_AREAS")]<-"No affected areas"
dat_RCP$variable[which(dat_RCP$variable=="NEW_AREAS")]<-"New areas"

dat_RCP$variable<-factor(dat_RCP$variable,c("Lost areas",
                                                      "No affected areas",
                                                      "New areas"))

dat_RCP$variable<-factor(dat_RCP$variable)

p<- ggplot(dat_RCP,aes(x=RCP_SCENARIO, y=value, fill=variable)) +
  geom_bar(stat = "identity", aes(fill = variable), position = "dodge")+
 # geom_boxplot(outlier.size=NA)+
  #stat_boxplot(geom ='errorbar') +
  #     stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
  #     stat_summary(fun.y=mean, geom="point")+
  xlab("Scenario")+
  ylab("Pixels")+
  #ggtitle(vars_labels2[[j]])+
  theme(panel.background = element_rect(fill = "gray90"),
        text=element_text(size=45),
        #axis.text.x  =element_blank(),
        axis.text.x  = element_text(size=45,colour="black"),
        axis.title=element_text(size=45,face="bold"),
        axis.text.y  = element_text(size=45,colour="black"),
        legend.position="bottom",
        legend.text = element_text(colour="black", size = 33),
        # legend.justification=c(1,0), 
        #legend.position=c(0.96,0.65),
        legend.key.size = unit(8,"line"),
        legend.title=element_blank()
  ) #+
  #labs(fill = "")

ggsave(paste0(out_dir,"/","COMBINED","_RCP_AFFECTED_AREAS_",Sys.Date(),".pdf"),p,units="in",width=90,height=50,scale=1,dpi=400,limitsize = FALSE);gc()

}
