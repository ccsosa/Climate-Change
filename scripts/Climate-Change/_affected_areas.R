require(raster);require(FactoMineR);require(factoextra)

afffected_areas_function<-function(graphics_dir,current_Out_Dir,ens_dir,RCPs,GCMS,PERIOD,grid_dir,var_cho,sp_Dir_Original){
  
  ####################################################################################################################
  ####################################################################################################################
  #calling baseline thr file
  
  cat("Calling thresholded current SDM","\n")
  bl_prob_thr<-raster(paste0(current_Out_Dir,"/","MCAA_FINAL_THR.tif"))
  
  bl_prob_thr_mask<-bl_prob_thr
  bl_prob_thr[which(is.na(bl_prob_thr[]))]<-0

  ####################################################################################################################
  ####################################################################################################################
  ###CALLING PROJECTED FILES
  
 
  ###PROJECTING DIR     
  out_dir<-sp_Dir_Original
  ##########################################################
  ##################################################
  ###CLIMATE LAYERS
  
  input_dir<-grid_dir
  
  RCP_dir<-paste0(grid_dir,"/",RCPs)
  
  GCM_dir<-unlist(lapply(1:length(RCP_dir),function(i){
    
    x<-paste0(RCP_dir[[i]],"/",GCMS)
    
    return(x)
  }))
  
  
  
  PERIOD_dir<-unlist(lapply(1:length(GCM_dir),function(i){
    
    x<-paste0(GCM_dir[[i]],"/",PERIODS)
    
    return(x)
  }))
  
  target_dir<-paste0(PERIOD_dir,"/","biovars")
  #########################################################
  #PREPARING SCENARIOS TO BE ANALYZED
  ad<-PERIOD_dir;ad<-sub(paste0(input_dir,"/"),"",ad);ad<-sub("/","_",ad);ad<-sub("/","_",ad);
  ad2<-PERIOD_dir;ad2<-sub(paste0(input_dir,"/"),"",ad2)
  
  for(i in 1:length(GCMS)){
    ad2<-sub(paste0("/",GCMS[[i]],"/"),"/",ad2)
    
  }
  ad2<-sub("/"," | ",ad2)
  
  
  
  proj_dir<-paste0(sp_Dir_Original,"/","projection")
  
  RCP_proj_dir<-paste0(proj_dir,"/",RCPs)
  
  GCM_proj_dir<-unlist(lapply(1:length(RCP_proj_dir),function(i){
    
    x<-paste0(RCP_proj_dir[[i]],"/",GCMS)
    
    return(x)
  }))
  
  
  PERIOD_proj_dir<-unlist(lapply(1:length(GCM_proj_dir),function(i){
    
    x<-paste0(GCM_proj_dir[[i]],"/",PERIODS)
    
    return(x)
  }))
  
  
  
  cat("Calling all thresholded SDMs for Migration scenario","\n")
  
  target_var_MIG<- paste0(PERIOD_proj_dir,"/","MCAA_FINAL_THR.tif")
  
  cat("Calling all thresholded SDMs for NO Migration scenario","\n")
  
target_var_NO_MIG<- paste0(PERIOD_proj_dir,"/","MCAA_FINAL_THR_NO_MIG.tif")

  
  ############
layer_gcms<-lapply(1:length(target_dir),function(i){
  
  cat(i," of ",length(target_dir)," | ",(i/length(target_dir)*100)," %","\n")

  layers_gcm<-stack(lapply(paste0(target_dir[[i]],"/",var_cho,".tif"),raster))
 
  return(layers_gcm)
});gc()


target_var_MIG_layers<-lapply(1:length(target_var_MIG),function(i){
  
  cat(i," of ",length(target_var_MIG)," | ",(i/length(target_var_MIG)*100)," %","\n")
  
  x<-raster(target_var_MIG[[i]])
  
  return(x)
});gc()


target_varNO__MIG_layers<-lapply(1:length(target_var_NO_MIG),function(i){
  
  cat(i," of ",length(target_var_NO_MIG)," | ",(i/length(target_var_NO_MIG)*100)," %","\n")
  
  x<-raster(target_var_NO_MIG[[i]])
  
  return(x)
});gc()
  



##########################################################
###GETTING AFFECTED AREAS
cat("                                           ","\n")
cat("Getting affected areas (MIGRATION SCENARIO)","\n")
cat("                                           ","\n")
AFFECTED_AREAS_MIG<-lapply(1:length(target_var_MIG_layers),function(i){
  cat("(MIGRATION SCENARIO)",i," of ",length(target_var_MIG_layers),
      " | ",(i/length(target_var_MIG_layers)*100)," %","\n")
  
x<-target_var_MIG_layers[[i]]

y<-raster::overlay(x,bl_prob_thr,fun=function(x, bl_prob_thr){
  x[which(x[]==1)]<-2
  x[which(is.na(x[]))]<-0
  y<-x-bl_prob_thr
  y[which(y[]==0)]<-NA
  return(y)})

return(y)

});gc()




AFFECTED_AREAS_MIG_list_csv<-lapply(1:length(AFFECTED_AREAS_MIG),function(i){
  cat("SUMMARY (MIGRATION SCENARIO)",i," of ",length(AFFECTED_AREAS_MIG),
      " | ",(i/length(AFFECTED_AREAS_MIG)*100)," %","\n")
  
  
  x<-AFFECTED_AREAS_MIG[[i]]
  y<-as.data.frame(matrix(nrow=1,ncol=6))
  colnames(y)<-c("GCM","RCP_SCENARIO","SCENARIO","NEW_AREAS","KEPT_AREAS","LOST_AREAS")
  y[,1]<-ad[[i]]
  y[,2]<-ad2[[i]]
  y[,3]<-"MIGRATION"
  y[,4]<-length(x[][which(x[]==2)])
  y[,5]<-length(x[][which(x[]==1)])
  y[,6]<-length(x[][which(x[]==-1)])
  return(y)
  
});gc()

AFFECTED_AREAS_MIG_list_csv<-do.call(rbind,AFFECTED_AREAS_MIG_list_csv)
######################################################################  
AFFECTED_AREAS_MIG<-stack(AFFECTED_AREAS_MIG)
out_dir<-paste0(sp_Dir_Original,"/","ensemble")

writeRaster(AFFECTED_AREAS_MIG,paste0(out_dir,"/","affected_areas_all.img"),overwrite=F)

  ####################################################################################################################
  ####################################################################################################################
  require(reshape2)
dat.m <- melt(AFFECTED_AREAS_MIG_list_csv,id.vars=c('GCM',"RCP_SCENARIO"), measure.vars=c("NEW_AREAS","KEPT_AREAS","LOST_AREAS"))
#dat.m<-dat.m[complete.cases(dat.m),]

vars<-c("NEW_AREAS","KEPT_AREAS","LOST_AREAS")
vars_labels<-c("New areas available (Pixels)","No affected areas (Pixels)","Lost areas (Pixels)")
p<-list()
for(i in 1:length(vars)){
  x<-dat.m[which(dat.m$variable==vars[[i]]),]

  p[[i]]<- ggplot(x,aes(x=RCP_SCENARIO, y=value, fill=RCP_SCENARIO)) +
    geom_boxplot(aes(RCP_SCENARIO),outlier.size=NA,position=position_dodge(width=6),x)+
    stat_boxplot(geom ='errorbar') +
    stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
    stat_summary(fun.y=mean, geom="point")+
    xlab("Scenario")+
    ylab(vars_labels[[i]])+
      theme(panel.background = element_rect(fill = "gray90"),
            text=element_text(size=45),
            #axis.text.x  =element_blank(),
            axis.text.x  = element_text(size=45,colour="black"),
            axis.title=element_text(size=45,face="bold"),
            axis.text.y  = element_text(size=45,colour="black"),
            legend.position="none") #+

   ggsave(paste0(graphics_dir,"/",vars[[i]],"_",Sys.Date(),".pdf"),p[[i]],units="in",width=54,height=36,scale=1,dpi=400,limitsize = FALSE)
}


write.csv(AFFECTED_AREAS_MIG_list_csv,paste0(graphics_dir,"/","AFFECTED_AREAS_MIG_csv",".csv"),row.names=F,quote=F)


####################################################################################################################
####################################################################################################################
  #CALLING CLIMATE LAYERS
  
  RCP_dir<-paste0(grid_dir,"/",RCPs)
  #GCM_dir<-paste0(RCP_dir,"/",GCM)
  PERIOD_dir<-paste0(GCM_dir,"/",PERIODS)
  
#########################################################
#PREPARING SCENARIOS TO BE ANALYZED
# ad<-PERIOD_dir;ad<-sub(paste0(input_dir,"/"),"",ad);ad<-sub("/","_",ad);ad<-sub("/","_",ad);
# ad2<-PERIOD_dir;ad2<-sub(paste0(input_dir,"/"),"",ad2)
# 
# for(i in 1:length(GCMS)){
#   ad2<-sub(paste0("/",GCMS[[i]],"/"),"/",ad2)
#   
# }
# ad2<-sub("/"," | ",ad2)
#   
  ##########################################################
  ###CURRENT CLIMATE LAYERS


areas_val<-c(-1,1,2)
AFFECTED_AREAS_MIG_NEW<-lapply(1:nlayers(AFFECTED_AREAS_MIG),function(i){
  cat("               ","\n")
  cat(i,"\n")
  
  x<-AFFECTED_AREAS_MIG[[i]]
  
  
  matrix_clim<-lapply(1:length(areas_val),function(j){
    cat(i," | ",j,"\n")
    areas_val_esp<-areas_val[[j]]
  
  clim<-lapply(1:nlayers(layer_gcms[[i]]),function(k){
    cat(i," | ",j," | ",k,"\n")
    x[which(x[]!=areas_val_esp)]<-NA
    x[which(x[]==areas_val_esp)]<-1
    xy<-layer_gcms[[i]][[k]]*x
    xy[which(xy[]==0)]<-NA
    return(xy)
    
  })
  clim<-stack(clim)
  names(clim)<-var_cho
  y<-as.data.frame(rasterToPoints(clim))
  y$RCP_SCENARIO<-NA
  y$GCM<-NA
  y$SCENARIO<-NA
  y$AREA_SCE<-NA
  #
  y$RCP_SCENARIO<-ad2[[i]]
  y$GCM<-ad[[i]]
  y$SCENARIO<-"MIGRATION"
  y$AREA_SCE<-as.factor(areas_val_esp)
return(y)  
  
    })

fm_mat<-do.call(rbind,matrix_clim)
return(fm_mat)
cat("               ","\n")

  });gc()


AFFECTED_AREAS_MIG_NEW<-do.call(rbind,AFFECTED_AREAS_MIG_NEW);gc()

current_clim_layer<-lapply(paste0(current_clim_dir,"/","/",paste0(var_cho,".tif")),raster)
current_clim_layer<-stack(current_clim_layer)


current_clim_layer2<-current_clim_layer*bl_prob_thr_mask
names(current_clim_layer2)<-var_cho
y_current<-as.data.frame(rasterToPoints(current_clim_layer2))
y_current$RCP_SCENARIO<-NA
y_current$GCM<-NA
y_current$SCENARIO<-NA
y_current$AREA_SCE<-NA
#
y_current$RCP_SCENARIO<-"CURRENT"
y_current$GCM<-"CURRENT"
y_current$SCENARIO<-"CURRENT"
y_current$AREA_SCE<-as.factor(1)



#AFFECTED_AREAS_MIG_NEW2<-do.call(rbind,AFFECTED_AREAS_MIG_NEW);gc()
AFFECTED_AREAS_MIG_NEW<-rbind(y_current,AFFECTED_AREAS_MIG_NEW);gc()
  ##########################################################
  ###BOXPLOTS

write.csv(AFFECTED_AREAS_MIG_NEW,paste0(sp_Dir_Original,"/","ensemble","/","affected_areas_all.csv"),quote=F,row.names=F)

####################################################################################################################
####################################################################################################################
require(reshape2)
#dat.m<-dat.m[complete.cases(dat.m),]
areas_val
vars_labels2<-c(
  "Lost areas (Pixels)",
  "No affected areas (Pixels)",
  "New areas available (Pixels)"
  )


vars2<-c(
  "LOST_AREAS",
  "KEPT_AREAS",
  "NEW_AREAS"

)

cl_an_dir<-paste0(graphics_dir,"/","climate_analysis");if(!file.exists(cl_an_dir)){dir.create(cl_an_dir)}
p<-list()





for(j in 1:length(areas_val)){
  val<-areas_val[[j]]
  dat.m_N<-AFFECTED_AREAS_MIG_NEW[which(AFFECTED_AREAS_MIG_NEW$AREA_SCE==val),];gc()
  dat.m_N <- melt(dat.m_N[,-c(1:2)],id.vars=c('GCM',"RCP_SCENARIO"), measure.vars=var_cho);gc()
  
for(i in 1:length(var_cho)){
  x<-dat.m_N[dat.m_N$variable==var_cho[[i]],]
   p[[i]]<- ggplot(x,aes(x=RCP_SCENARIO, y=value, fill=RCP_SCENARIO)) +
    geom_boxplot(aes(RCP_SCENARIO),outlier.size=NA,position=position_dodge(width=6),x)+
    stat_boxplot(geom ='errorbar') +
    stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
    stat_summary(fun.y=mean, geom="point")+
    xlab("Scenario")+
    ylab(var_cho[[i]])+
     ggtitle(vars_labels2[[j]])+
    theme(panel.background = element_rect(fill = "gray90"),
          text=element_text(size=45),
          #axis.text.x  =element_blank(),
          axis.text.x  = element_text(size=45,colour="black"),
          axis.title=element_text(size=45,face="bold"),
          axis.text.y  = element_text(size=45,colour="black"),
          legend.position="none") #+
  
  ggsave(paste0(cl_an_dir,"/",vars2[[j]],"_",var_cho[[i]],"_",Sys.Date(),".pdf"),p[[i]],units="in",width=54,height=36,scale=1,dpi=400,limitsize = FALSE)
     };rm(i)
};rm(j)
  ##########################################################
  ###COMBINED CLIMATE BOXPLOTS
  

vars_labels2<-c(
  "Lost areas (Pixels)",
  "No affected areas (Pixels)",
  "New areas available (Pixels)"
)
AFFECTED_AREAS_MIG_NEW2<-AFFECTED_AREAS_MIG_NEW
AFFECTED_AREAS_MIG_NEW2$AREA_SCENARIO<-NA

AFFECTED_AREAS_MIG_NEW2$AREA_SCENARIO[which(AFFECTED_AREAS_MIG_NEW2$AREA_SCE==-1)]<-"Lost areas"
AFFECTED_AREAS_MIG_NEW2$AREA_SCENARIO[which(AFFECTED_AREAS_MIG_NEW2$AREA_SCE==1)]<-"No affected areas"
AFFECTED_AREAS_MIG_NEW2$AREA_SCENARIO[which(AFFECTED_AREAS_MIG_NEW2$AREA_SCE==2)]<-"New areas"
  
dat_AF_ARm_N<-AFFECTED_AREAS_MIG_NEW2[,c(var_cho,"RCP_SCENARIO","AREA_SCENARIO")]
  
  
 dat.m_N <- melt(dat_AF_ARm_N,id.vars=c("AREA_SCENARIO","RCP_SCENARIO"), measure.vars=var_cho);gc()
# dat.m_N<-dat.m_N[order(dat.m_N$AREA_SCENARIO== "Lost areas (Pixels)",
#                        dat.m_N$AREA_SCENARIO== "No affected areas (Pixels)",
#                        dat.m_N$AREA_SCENARIO== "New areas available (Pixels)"
#                        ),]

dat.m_N$AREA_SCENARIO<-factor(dat.m_N$AREA_SCENARIO,c("Lost areas",
                                                      "No affected areas",
                                                      "New areas"))

dat.m_N$RCP_SCENARIO<-factor(dat.m_N$RCP_SCENARIO)



for(i in 1:length(var_cho)){
  x<-dat.m_N[dat.m_N$variable==var_cho[[i]],]
  p[[i]]<- ggplot(x,aes(x=RCP_SCENARIO, y=value, fill=AREA_SCENARIO)) +
    geom_boxplot(outlier.size=NA)+
    stat_boxplot(geom ='errorbar') +
#     stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
#     stat_summary(fun.y=mean, geom="point")+
    xlab("Scenario")+
    ylab(var_cho[[i]])+
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
          legend.key.size = unit(15,"line")
          ) +
   labs(fill = "")
  
  ggsave(paste0(cl_an_dir,"/","COMBINED","_",var_cho[[i]],"_",Sys.Date(),".pdf"),p[[i]],units="in",width=90,height=50,scale=1,dpi=400,limitsize = FALSE);gc()
};rm(i)


head(dat_AF_ARm_N)
dat_AF_ARm_N$SCENARIO<-NA
dat_AF_ARm_N$SCENARIO<-paste0(dat_AF_ARm_N$RCP_SCENARIO,"_|_",dat_AF_ARm_N$AREA_SCENARIO)
dat_AF_ARm_N2<-dat_AF_ARm_N
dat_AF_ARm_N2<-dat_AF_ARm_N2[complete.cases(dat_AF_ARm_N2),];gc()

cat("Calculating PCA for extracted pixels","\n")
ss<-PCA(dat_AF_ARm_N2[,var_cho],scale.unit = T,graph = F);gc()

#ss2<-HCPC(ss,nb.clust=-1,iter.max=1000,graph = F);gc()



g1<-fviz_pca_biplot(ss, label="var", habillage=as.factor(dat_AF_ARm_N2$AREA_SCENARIO), addEllipses=T,
                    ellipse.level=0.95,repel = T,labelsize = 18,ellipse.type = "convex",
                    title = "RCP - PERIOD AFFECTED AREAS PCA",
                    alpha.ind=0,
                    legend.title = "Scenarios") +
  
  theme(panel.background = element_rect(fill = "gray90"),
        text=element_text(size=45),
        #axis.text.x  =element_blank(),
        axis.text.x  = element_text(size=45,colour="black"),
        axis.title=element_text(size=45,face="bold"),
        axis.text.y  = element_text(size=45,colour="black"),
        legend.text = element_text(colour="black", size = 33),
        legend.justification=c(1,0), legend.position=c(0.96,0.65),
        legend.key.size = unit(6,"line")
        #legend.position="right"
  );gc() #+


ggsave(paste0(cl_an_dir,"/","RCP_PERIOD_CLIM_PCA","_",Sys.Date(),".pdf"),g1,
       units="in",width=90,height=60,scale=1,dpi=400,limitsize = FALSE)

sum_p<-ss$eig
sums<-dimdesc(ss, axes=c(1,2))
write.csv(sums$Dim.1$quanti,paste0(cl_an_dir,"/","PC1.csv"))
write.csv(sums$Dim.2$quanti,paste0(cl_an_dir,"/","PC2.csv"))
write.csv(sum_p,paste0(cl_an_dir,"/","PC_cum_var.csv"))

#rm(g1)
cat("     ","\n")
cat("#####","\n")
cat("DONE!","\n")
cat("DONE!","\n")
cat("#####","\n")
}
