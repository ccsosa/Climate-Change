require(raster);require(berryFunctions);require(FactoMineR);require(ggplot2);require(factoextra)
require(gplots);library(reshape2)
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")

STEPWISE_STEP_FUNCTION<-function(sp_Dir_Original,current_Out_Dir,GCMS,PERIODS,RCPs,grid_dir,spData_presence){

graphics_dir<-paste0(sp_Dir_Original,"/","Graphics");if(!file.exists(graphics_dir)){dir.create(graphics_dir)}
##########################################################
###CURRENT MCAA VALUES
bl_prob<-raster(paste0(current_Out_Dir,"/","MCAA_FINAL.tif"))


#var_cho<-nipals_by_specie(spData_presence)


##########################################################
###CURRENT CLIMATE LAYERS
current_clim_layer<-lapply(paste0(current_clim_dir,"/","/",paste0(var_cho,".tif")),raster)
current_clim_layer<-stack(current_clim_layer)


##################################################
###CURRENT


data<-as.data.frame(extract(stack(bl_prob,current_clim_layer),spData_presence))
data$GCM<-"Current"

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


target_var_MIG<- paste0(PERIOD_proj_dir,"/","MCAA_FINAL.tif")

##################################################
#EXTRACTING CLIMATE INFO
cat("Extracting climate information","\n")
layer_gcms<-lapply(1:length(target_dir),function(i){
  
  cat(i," of ",length(target_dir)," | ",(i/length(target_dir)*100)," %","\n")
  
  MCAA_FINAL<-raster(target_var_MIG[[i]])
  layers_gcm<-stack(lapply(paste0(target_dir[[i]],"/",var_cho,".tif"),raster))
  
  layers<-stack(MCAA_FINAL,layers_gcm)
  data<-as.data.frame(extract(layers,spData_presence))
  data$GCM<-ad[[i]]
  return(data)
  });gc()


data_period_gcm<-do.call(rbind,layer_gcms);gc()
data_period<-rbind(data,data_period_gcm);gc()


##################################################
#EXTRACTING MEDIANS FROM CLIMATE FILE
cat("Calculating median file to use","\n")

data_mean<-as.data.frame(matrix(nrow=length(unique(data_period$GCM)),ncol=ncol(data_period)))
colnames(data_mean)<-colnames(data_period)
row.names(data_mean)<-unique(data_period$GCM)
data_mean$GCM<-unique(data_period$GCM)
for(i in 1:(ncol(data_mean)-1)){
data_mean[,i]<-as.numeric(tapply(data_period[,i],data_period$GCM,median))
};rm(i)

##data_mean$RCP<-NA

#rcp_names<-(strsplit(data_mean$GCM,split = GCMS))
rcp_names<-c("Current",ad2)
# rcp_names
# 
# rcp_names<-sub(GCMS,"",data_mean$GCM)
# rcp_names<-(strsplit(data_mean$GCM,split = paste0("_",GCMS,"_")))
# rcp_names<-unlist(lapply(rcp_names,function(x){
#   x[[length(x)]]
# })
# )

##################################################
#PCA
cat("Calculating PCA","\n")
ss<-PCA(data_mean[,-c(1,ncol(data_mean))],scale.unit = T,graph = F)

ss2<-HCPC(ss,nb.clust=-1,iter.max=1000,graph = F)


con1<-fviz_contrib(ss, choice = "var", axes = 1, top = 10)+
  theme(panel.background = element_rect(fill = "gray90"),
        text=element_text(size=45),
        #axis.text.x  =element_blank(),
        axis.text.x  = element_text(size=45,colour="black"),
        axis.title=element_text(size=45,face="bold"),
        axis.text.y  = element_text(size=45,colour="black"))
ggsave(paste0(graphics_dir,"/","PC1_CONTRIBUTION","_",Sys.Date(),".pdf"),con1,units="in",width=54,height=36,scale=1,dpi=400,limitsize = FALSE)

con2<-fviz_contrib(ss, choice = "var", axes = 2, top = 10)+
  theme(panel.background = element_rect(fill = "gray90"),
        text=element_text(size=45),
        #axis.text.x  =element_blank(),
        axis.text.x  = element_text(size=45,colour="black"),
        axis.title=element_text(size=45,face="bold"))
ggsave(paste0(graphics_dir,"/","PC2_CONTRIBUTION","_",Sys.Date(),".pdf"),con2,units="in",width=54,height=36,scale=1,dpi=400,limitsize = FALSE)


sum_p<-ss$eig
sums<-dimdesc(ss, axes=c(1,2))
write.csv(sums$Dim.1$quanti,paste0(graphics_dir,"/","PC1.csv"))
write.csv(sums$Dim.2$quanti,paste0(graphics_dir,"/","PC2.csv"))
write.csv(sum_p,paste0(graphics_dir,"/","PC_cum_var.csv"))

#data_mean$RCP<-rcp_names

rcp_names_2<-rcp_names
for(i in 1:length(PERIODS)){
  rcp_names_2<-gsub(PERIODS[[i]],"",rcp_names_2)
    
}

rcp_names_2<-(strsplit(rcp_names_2,split = " "))

rcp_names_2<-unlist(lapply(rcp_names_2,function(x){
  
  x[[1]]
})
)
########################################################################
########################################################################
########################################################################

g1<-fviz_pca_biplot(ss, label="var", habillage=as.factor(rcp_names), addEllipses=T,
                ellipse.level=0.95,repel = T,labelsize = 18,ellipse.type = "convex",
                title = "RCP - PERIOD PCA",
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
      ) #+




ggsave(paste0(graphics_dir,"/","RCP_PERIOD_PCA","_",Sys.Date(),".pdf"),g1,
       units="in",width=48,height=36,scale=1,dpi=400,limitsize = FALSE)
########################################################################
########################################################################
########################################################################
g2<-fviz_pca_biplot(ss, label="var", habillage=as.factor(rcp_names_2), addEllipses=T,
                    ellipse.level=0.95,repel = T,labelsize = 18,ellipse.type = "convex",
                    title = "RCP - PERIOD PCA",
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
  ) #+

ggsave(paste0(graphics_dir,"/","RCP_PCA","_",Sys.Date(),".pdf"),g2,
       units="in",width=48,height=36,scale=1,dpi=400,limitsize = FALSE)

########################################################################
########################################################################
########################################################################
  
clust_ind<-as.data.frame(as.character(ss2$data.clust$clust))
clust_ind$Scenario<-as.character(row.names(ss2$data.clust))
colnames(clust_ind)<-c("GROUP","SCENARIO")
clust_ind$GROUP<-as.character(clust_ind$GROUP)
clust_ind$GROUP[which(clust_ind$SCENARIO=="Current")]<-"Current"

g3<-fviz_pca_biplot(ss, label="var", habillage=factor(clust_ind$GROUP), addEllipses=T,
                ellipse.level=0.95,repel = T,labelsize = 18,ellipse.type = "convex",
                title = "RCP PCA",
                legend.title = "Scenarios",
) + 

  theme(panel.background = element_rect(fill = "gray90"),
        text=element_text(size=45),
        #axis.text.x  =element_blank(),
        axis.text.x  = element_text(size=45,colour="black"),
        axis.title=element_text(size=45,face="bold"),
        axis.text.y  = element_text(size=45,colour="black"),
        legend.text = element_text(colour="black", size = 33),
        legend.justification=c(1,0), legend.position=c(0.96,0.65),
        legend.key.size = unit(6,"line")
)


        ggsave(paste0(graphics_dir,"/","CLUSTERING_PCA","_",Sys.Date(),".pdf"),g3,
               units="in",width=48,height=36,scale=1,dpi=400,limitsize = FALSE)
        



da_clust<-as.data.frame(cbind(data_mean,clust_ind))




########################################################################
########################################################################
cat("saving metrics for clusters","\n")


clus<-unique(da_clust$GROUP)

tmp<-list()
#i=1
for(i in 1:length(clus)){
  
  df3<-da_clust[which(da_clust$GROUP==clus[[i]]),]
  df3<-df3[,-c(seq(ncol(df3)-2,ncol(df3),1))]
  
  tmp[[i]]<-as.data.frame(matrix(nrow=ncol(df3),ncol=7))
  
  colnames(tmp[[i]])<-c("mean","sd","median","min","max","n","cluster")
  row.names(tmp[[i]])<-colnames(df3)
  for(j in 1:(ncol(df3))){
    
    if(is.factor(df3[[j]])) {
      cat("factor","/n")
      
      tmp[[i]][j,1]<-NA
      tmp[[i]][j,2]<-NA
      tmp[[i]][j,3]<-NA
      tmp[[i]][j,4]<-NA
      tmp[[i]][j,5]<-NA
      tmp[[i]][j,6]<-NA
      tmp[[i]][j,7]<-NA
      
      
      
      
    } else{
      tmp[[i]][j,1]<-mean(df3[[j]],na.rm=T)
      tmp[[i]][j,2]<-sd(df3[[j]],na.rm=T)
      tmp[[i]][j,3]<-median(df3[[j]],na.rm=T)
      tmp[[i]][j,4]<-min(df3[[j]],na.rm=T)
      tmp[[i]][j,5]<-max(df3[[j]],na.rm=T)
      tmp[[i]][j,6]<-length(df3[[j]])
      tmp[[i]][j,7]<-as.character(clus[[i]])
      
    }
  };rm(j)
};rm(i)



total<-as.data.frame(matrix(nrow=ncol(da_clust[,-c(seq(ncol(da_clust)-2,ncol(da_clust),1))]),ncol=7))

colnames(total)<-c("mean","sd","median","min","max","n","cluster")
row.names(total)<-colnames(df3)


for(i in 1:ncol(da_clust[,-c(seq(ncol(da_clust)-2,ncol(da_clust),1))])){
  
  if(is.factor(da_clust[,-c(seq(ncol(da_clust)-2,ncol(da_clust),1))][[i]])) {
    cat("factor","/n")
    
  } else{
    total[i,1]<-mean(da_clust[,-c(seq(ncol(da_clust)-2,ncol(da_clust),1))][[i]],na.rm=T)
    total[i,2]<-sd(da_clust[,-c(seq(ncol(da_clust)-2,ncol(da_clust),1))][[i]],na.rm=T)
    total[i,3]<-median(da_clust[,-c(seq(ncol(da_clust)-2,ncol(da_clust),1))][[i]],na.rm=T)
    total[i,4]<-min(da_clust[,-c(seq(ncol(da_clust)-2,ncol(da_clust),1))][[i]],na.rm=T)
    total[i,5]<-max(da_clust[,-c(seq(ncol(da_clust)-2,ncol(da_clust),1))][[i]],na.rm=T)
    total[i,6]<-length(da_clust[,-c(seq(ncol(da_clust)-2,ncol(da_clust),1))][[i]])
    total[i,7]<-"Total"
    
    
  }
};rm(i)


final<-do.call("rbind",tmp)

final<-rbind(total,final)
write.table(final,paste0(graphics_dir,"/","CLUSTERING_PCA_SUMMARY","_",Sys.Date(),".csv"),quote=F,row.names = T,sep = ",")



########################################################################
########################################################################



########################################################################





  #scale_x_continuous(breaks=c(-2:2,4:8),labels=c(-2:2,-2:2))
# fviz_pca_ind(ss,
#              col.ind = ss2$data.clust$clust,
#              #col.var = as.factor(ss2$data.clust$clust),
#              #label = , # hide individual labels
#              #habillage =ss2$data.clust$clust, # color by groups
#              #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#              addEllipses = TRUE,
#              repel = TRUE# Concentration ellipses
# )

########################################################
#STEPWISE

##################################################
#PCA
cat("Calculating STEPWISE REGRESSION","\n")
cat("                                ","\n")
cat("Ommiting estimators with p value >0.05","\n")
#write.csv(cu_stp_1, paste0(out_dir,"/","Stepwise_current.csv"), row.names=T)

GCM_TO_LIST<-unique(data_period$GCM)

data_f_stepwise<-lapply(1:length(GCM_TO_LIST),function(i){
  cat(i," of ", "%","\n")
  
  X<-subset(data_period,data_period$GCM==GCM_TO_LIST[[i]])
  model_1<-glm(MCAA_FINAL~., data=X[,-ncol(X)])
  model_2<-step(model_1, direction="both")
  cu_stp<-summary(model_2)
  cu_stp_1 <- cu_stp$coefficients
  cu_stp_1 <- as.data.frame(t(cu_stp_1))
  
  ####
  data_f_Current<-as.data.frame(matrix(ncol=length(c("GCM","VARIABLE","INTERCEPT",var_cho)),nrow=4))
  colnames(data_f_Current)<-c("GCM","VARIABLE","INTERCEPT",var_cho)
  data_f_Current[,1]<-GCM_TO_LIST[[i]]
  data_f_Current[,2]<-row.names(cu_stp_1)
  data_f_Current[,3]<-cu_stp_1[,1]
  
  for(i in 4:(ncol(data_f_Current))){
    if(berryFunctions::is.error(cu_stp_1[,var_cho[(i-3)]])){
      data_f_Current[,i]<-NA
    }else{
      data_f_Current[,i] <-cu_stp_1[,var_cho[(i-3)]]
      
    }
  }
  
  
  ####
  return(data_f_Current)
  
})


data_f_stepwise<-do.call(rbind,data_f_stepwise)


data_f_stepwise2<-data_f_stepwise



data_f_stepwise2<-lapply(1:length(GCM_TO_LIST),function(i){
  X<-subset(data_f_stepwise,data_f_stepwise$GCM==GCM_TO_LIST[[i]])
  cat("i: ",i,"\n")
  for(j in 4:ncol(X)){
    cat("j: ",j,"\n")
    
    
    if(is.na(X[4,j])){
      cat("...","\n") 
    }else if(X[4,j]>=.05){
      
      X[1,j]<-NA
      }else{
        cat("...","\n")
      }
   }
  return(X)
})

data_f_stepwise2<-do.call(rbind,data_f_stepwise2)
data_f_stepwise3<-data_f_stepwise2[which(data_f_stepwise2$VARIABLE=="Estimate"),-c(2,3)]
data_f_stepwise3$SCENARIOS<-rcp_names
data_f_stepwise3<-data_f_stepwise3[,c("SCENARIOS","GCM",var_cho)]

dat.m <- melt(data_f_stepwise3,id.vars=c('GCM',"SCENARIOS"), measure.vars=var_cho)
#dat.m<-dat.m[complete.cases(dat.m),]


p<-list()
for(i in 1:length(var_cho)){
  x<-dat.m[which(dat.m$variable==var_cho[[i]]),]

  
  p[[i]] <- ggplot(x,aes(x=SCENARIOS, y=value, fill=SCENARIOS)) +
    geom_boxplot(aes(SCENARIOS),outlier.size=NA,position=position_dodge(width=6),x)+
    stat_boxplot(geom ='errorbar') +
    stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
    stat_summary(fun.y=mean, geom="point")+
    geom_hline(yintercept = 0)+
    ggtitle(paste0("",var_cho[[i]]))+
    xlab("Scenario")+
    ylab("Estimator value")+
  #theme(legend.position="none")+
    theme(panel.background = element_rect(fill = "gray90"),
          text=element_text(size=45),
          #axis.text.x  =element_blank(),
          axis.text.x  = element_text(size=45,colour="black"),
          axis.title=element_text(size=45,face="bold"),
          axis.text.y  = element_text(size=45,colour="black"),
          legend.position="none") #+
   # coord_flip()
  
 # graphics_dir
ggsave(paste0(graphics_dir,"/",var_cho[[i]],"_",Sys.Date(),".pdf"),p[[i]],units="in",width=54,height=36,scale=1,dpi=400,limitsize = FALSE)
  
}
cat("Saving stepwise outcomes","\n")
write.csv(data_f_stepwise,paste0(graphics_dir,"/","STEPWISE_VALUES",".csv"),row.names=F,quote=F)
write.csv(data_mean,paste0(graphics_dir,"/","DATA_TO_PCA",".csv"),row.names=F,quote=F)
write.csv(data_period,paste0(graphics_dir,"/","DATA_TO_ANALYSIS",".csv"),row.names=F,quote=F)

##################################
cat("Boxplots per predictor and MCAA for occurrences","\n")


data_period2<-data_period

data_period2$Scenario<-NA


#scen<-unique(data_f_stepwise3$SCENARIOS)
for(i in 1:nrow(data_f_stepwise3)){
  
  data_period2$Scenario[which(data_period2$GCM==data_f_stepwise3$GCM[[i]])]<-
    data_f_stepwise3$SCENARIOS[[i]]
  
}

data_period_melt<-melt(data_period2,id.vars=c('GCM',"Scenario"), measure.vars=c("MCAA_FINAL",var_cho))

p<-list()
var_cho_plus<-c("MCAA_FINAL",var_cho)

for(i in 1:length(var_cho_plus)){
  x<-data_period_melt[which(data_period_melt$variable==var_cho_plus[[i]]),]
  
  
  p[[i]] <- ggplot(x,aes(x=Scenario, y=value, fill=Scenario)) +
    geom_boxplot(aes(Scenario),outlier.size=NA,position=position_dodge(width=6),x)+
    stat_boxplot(geom ='errorbar') +
    stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
    stat_summary(fun.y=mean, geom="point")+
    geom_hline(yintercept = 0)+
    ggtitle(paste0("",var_cho_plus[[i]]))+
    xlab("Scenario")+
    ylab("Value")+
    #theme(legend.position="none")+
    theme(panel.background = element_rect(fill = "gray90"),
          text=element_text(size=45),
          #axis.text.x  =element_blank(),
          axis.text.x  = element_text(size=45,colour="black"),
          axis.title=element_text(size=45,face="bold"),
          axis.text.y  = element_text(size=45,colour="black"),
          legend.position="none") #+
  # coord_flip()
  
  # graphics_dir
  ggsave(paste0(graphics_dir,"/",var_cho_plus[[i]],"_CLIMATE_",Sys.Date(),".pdf"),p[[i]],units="in",width=54,height=36,scale=1,dpi=400,limitsize = FALSE)
  
      }
cat("     ","\n")
cat("#####","\n")
cat("DONE!","\n")
cat("DONE!","\n")
cat("#####","\n")
}