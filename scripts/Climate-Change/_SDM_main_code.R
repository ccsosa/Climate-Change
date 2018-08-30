####################################################################################################################
####################################################################################################################
####################################################################################################################
############################################################    SDM USING SDM PACKAGE    ###########################
###########################################################       CHRYSTIAN C. SOSA         ########################
###########################################################             2017                       #################
####################################################################################################################
####################################################################################################################
####################################################################################################################

library(raster)
library(rgdal)
library(rJava)
library(dismo)
library(sdm)
library(parallel)
library(devtools)
#install_github('johnbaums/rmaxent')

library(rmaxent)
##########################################################
##PREVIOUS STEPS##
##########################################################

inDir<-"//dapadfs/workspace_cluster_6/CWR/CWR_PROJECT_CC_BD/ccsosa/CLIMATE_CHANGE"
bacK_Dir<-paste0(inDir,"/","swd");if(!file.exists(bacK_Dir)){dir.create(bacK_Dir)}
occ_Dir<-paste0(inDir,"/","_occurrences");if(!file.exists(occ_Dir)){dir.create(occ_Dir)}
src.dir<-paste0(inDir,"/","scripts/Climate-Change") 
mod_Dir<-paste0(inDir,"/","_modelling");if(!file.exists(mod_Dir)){dir.create(mod_Dir)}

##########################################################
###BASELINE DIRECTORY
##########################################################

input_dir <- "//dapadfs/workspace_cluster_6/CWR/CWR_PROJECT_CC_BD/ccsosa/CLIMATE_CHANGE/biolayers"
bl_dir <- paste0(input_dir,"/","current")
#current_clim_dir<-paste0(bl_dir,"/","biovars");if(!file.exists(paste0(bl_dir,"/","biovars"))){dir.create(paste0(bl_dir,"/","biovars"))}
current_clim_dir <- bl_dir#;if(!file.exists(paste0(bl_dir,"/","biovars"))){dir.create(paste0(bl_dir,"/","biovars"))}

##########################################################
###SWD FILES CREATING
##########################################################

source(paste0(src.dir,"/","_swd.R"))
#x<-swd_function(inDir=inDir,current_clim_dir=current_clim_dir,occ_Dir=occ_Dir,bg_create=T)
  
##########################################################
###CALLING SWD FILES
##########################################################
  
occs<-list.files(paste0(inDir,"/","swd"),".csv")
occFile<-paste0(bacK_Dir,"/",occs[[1]])
sp_name<-sub(bacK_Dir,"",occFile)
sp_name<-sub("/","",sp_name)
sp_name<-sub(".csv","",sp_name)

##########################################################

spData <- read.csv(occFile)
spData<-spData[,-c(1:3)]
spData_presence<-spData[which(spData$status==1),]



##########################################################
###SWD TO SPATIALPOINTDATAFRAME
##########################################################

coordinates(spData_presence)<-~lon+lat
crs(spData_presence)<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
## No spatial duplicates
spData_presence <- remove.duplicates(spData_presence)


##########################################################
###CHOOSE VARIABLES USING NIPALS
##########################################################

source(paste0(src.dir,"/","choose_variables.R"))

var_cho<-nipals_by_specie(spData_presence)

current_clim_layer<-lapply(paste0(current_clim_dir,"/","/",paste0(var_cho,".tif")),raster)
current_clim_layer<-stack(current_clim_layer)

spData_presence <- spData_presence[,c("status",var_cho)]
colnames(spData_presence@data)[1] <- "Sp"
##########################################################
###FORMATTING TO SDM PACKAGE FORMAT 
##########################################################

mod_Dir<-paste0(mod_Dir);if(!file.exists(mod_Dir)){dir.create(mod_Dir)}

sp_Dir<-paste0(mod_Dir,"/",sp_name);if(!file.exists(sp_Dir)){dir.create(sp_Dir)}
sp_Dir_Original<-sp_Dir
Eval_sp_Dir<-paste0(sp_Dir,"/","evaluation");if(!file.exists(Eval_sp_Dir)){dir.create(Eval_sp_Dir)}

#setwd(dir =sp_Dir )
current_Out_Dir<-paste0(sp_Dir,"/","current");if(!file.exists(current_Out_Dir)){dir.create(current_Out_Dir)}
#set.seed(1000)
##########################################################
if(!file.exists(paste0(sp_Dir,"/","sdm.sdm"))){
  frm <- as.formula(paste("Sp", "~", paste(names(current_clim_layer), sep = "+", collapse = "+"), "+coords(lon+lat)", sep = ""))
  
  data_sp_f <- sdm::sdmData(formula=frm,train=spData_presence,
                 #predictors=as.data.frame(spData_presence[,var_cho]),
                 predictors=current_clim_layer,
                    #bg=as.data.frame(spData_pseudo@coords))
                 bg=list(n=10000,method='gRandom',remove=TRUE)
                 );gc()
}else{
  
cat("Already modelled!","\n")  
}
##########################################################
###PERFORMING 5 SDMs ALGORITHMS
##########################################################

#m2@models$Sp$maxent$`1`@object



if(!file.exists(paste0(sp_Dir,"/","sdm.sdm"))){
m2 <- sdm(frm,data=data_sp_f,methods='maxent',#c('rf','brt','svm','glm','maxent'),
          replication='cv',
          test.percent=20,
          n=3,
          modelSettings = list(maxent = list(args='hinge=false', 'threshold=false')),
          var.selection=F,
          overwrite=F#,
          #nc=4
          );gc()

write.sdm(m2,paste0(sp_Dir,"/","sdm"))


}else{
  m2<-read.sdm(paste0(sp_Dir,"/","sdm.sdm"))
  
}
##########################################################
##########################################################
m2_eval<-getEvaluation(m2,opt=4, stat=c('AUC',
                                        'COR',
                                        'Deviance',
                                        'obs.prevalence',
                                        'threshold',
                                        'sensitivity',
                                        'specificity',
                                        'TSS',
                                        'Kappa',
                                        'NMI',
                                        'phi',
                                        'ppv',
                                        'npv',
                                        'ccr',
                                        'prevalence'
                                        ))

##########################################################
#GETTING EVALUATION INFO

mInfo<-sdm::getModelInfo(m2)

##########################################################
#NULL MODEL


if(!file.exists(paste0(Eval_sp_Dir,"/","metrics.csv"))){
source(paste0(src.dir,"/","_null_model.R"))
nAUC<-nullModel_calculate(spData_presence=spData_presence,current_clim_layer=current_clim_layer);gc()
}else{
  cat("Already modelled! OMMITING NULL MODEL","\n")  
  
}
##########################################################
###WRITING RASTER FILES TO PERFORM EVALUATION ANALYSIS
#########################################################

if(!file.exists(paste0(current_Out_Dir,"/","current_all.img"))){
  p2m_all <- lapply(1:nrow(m2_eval),function(i){
  cat(i)
  x <- pred_rmaxent <- rmaxent::project(m2@models$Sp$maxent[[i]]@object, current_clim_layer)
  x <- x$prediction_logistic
return(x)
    })
#detach("package:caret",unload = T)
# p2m <- predict(object=m2,
#                    newdata=current_clim_layer,
#                    w=1,
#                    filename=paste0(current_Out_Dir,"/","cur1.tif"),
#                    method="maxent",
#                    mean=F,
#                    overwrite=F,
#                    nc=5,
#                    object.size=50
#                   ) 
#   
   #p2m<-stack(brick(paste0(current_Out_Dir,"/","cur.img")))
  p2m_all <- stack(p2m_all)
names(p2m_all)<-paste0(mInfo$method,"_",mInfo$modelID)
writeRaster(p2m_all,paste0(current_Out_Dir,"/","current_all.img"));gc()
} else {
  p2m_all<-stack(brick(paste0(current_Out_Dir,"/","current_all.img")))  
  names(p2m_all)<-paste0(mInfo$method,"_",mInfo$modelID)
  
}

if(!file.exists(paste0(current_Out_Dir,"/","current.img"))){

#p2m_all <- predict(m2,newdata=current_clim_layer,filename=paste0(current_Out_Dir,"/","current_all.img"),nc=5,mean=F,overwrite=T);gc()
p2m <- mean(p2m_all,na.rm=T);gc()  #calc(p2m_all, median)#median(p2m_all,na.rm=T);gc()
writeRaster(p2m,paste0(current_Out_Dir,"/","current.img"))
}else{
  p2m <- raster(paste0(current_Out_Dir,"/","current.img"))
  names(p2m)<-unique(mInfo$method)

}

# if(!file.exists(paste0(current_Out_Dir,"/","current_all.img"))){
#   p2m_all <- predict(m2,newdata=current_clim_layer,filename=paste0(current_Out_Dir,"/","current_all.img"),method=("maxent"),nc=5,mean=T,overwrite=T);gc()
#   names(p2m_all)<-paste0(mInfo$method,"_",mInfo$modelID) 
#   }else{
#   p2m_all<-stack(brick(paste0(current_Out_Dir,"/","current_all.img")))
#   names(p2m_all)<-paste0(mInfo$method,"_",mInfo$modelID) 
# }
# if(!file.exists(paste0(current_Out_Dir,"/","current.img"))){
#   
# #p2m_all <- predict(m2,newdata=current_clim_layer,filename=paste0(current_Out_Dir,"/","current_all.img"),nc=5,mean=F,overwrite=T);gc()
# p2m <- mean(p2m_all,na.rm=T)
# writeRaster(p2m,paste0(current_Out_Dir,"/","current.img"))
# }else{
#   p2m <- raster(paste0(current_Out_Dir,"/","current.img"))
#   names(p2m)<-unique(mInfo$method)
#   
# }



##########################################################
###PERFORMING EVALUATION METRICS
######################################################## 
if(!file.exists(paste0(Eval_sp_Dir,"/","metrics.csv"))){
source(paste0(src.dir,"/","_evaluation.R"))
evaluation<-evaluation_function(m2_eval=m2_eval,mInfo=mInfo,nAUC=nAUC,p2m=p2m,p2m_all=p2m_all,current_Out_Dir=current_Out_Dir,cAUC_INC=F);gc()

write.csv(evaluation,paste0(Eval_sp_Dir,"/","metrics.csv"),quote=F,row.names=F)

}else{
  
  evaluation<-read.csv(paste0(Eval_sp_Dir,"/","metrics.csv"),header=T)
}

#if(sum(evaluation$valid_model)>0){
source(paste0(src.dir,"/","_evaluation_replicates.R"))
models_to_ensemble_MOD<-as.character(evaluation$METHOD[which(evaluation$valid_model==1)])

#if(!file.exists(paste0(Eval_sp_Dir,"/","metrics.csv"))){
  


m2_rep<-m2_rep_function(mInfo)
models_to_ensemble<-as.numeric(m2_rep$modelID[which(m2_rep$valid_model==1)])

source(paste0(src.dir,"/","00_evaluation_PCA.R"))


#rm(p2m,p2m_all)
##########################################################
###ENSAMBLING APPROACHES
######################################################## 

#CURRENT
#source(paste0(src.dir,"/","_evaluation_ens.R"))
sp_Dir<-current_Out_Dir

if(!file.exists(paste0(current_Out_Dir,"/","MCAA_FINAL_THR.tif"))){
#x<-eval_to_ens(models_to_ensemble,m2_eval,sp_Dir,mInfo,m2_rep,p2m_all);gc()
  p_raster <- p2m
  p_raster[which(p_raster[]>=median(m2_eval$threshold))]<-1
  p_raster[which(p_raster[]<median(m2_eval$threshold))]<-0
  
  writeRaster(p2m,paste0(sp_Dir,"/","MCAA_FINAL",".tif"))
  writeRaster(p_raster,paste0(sp_Dir,"/","MCAA_FINAL_THR",".tif"))
  }else{
  cat("Baseline already projected!","\n")
}
  gc()
##########################################################
###DEFINING RCPs
######################################################## 
RCPs<-c("rcp2_6","rcp4_5","rcp6_0","rcp8_5")

PERIODS<-c("2050s") #,"2080s"

grid_dir<-"//dapadfs/workspace_cluster_6/CWR/CWR_PROJECT_CC_BD/ccsosa/CLIMATE_CHANGE/biolayers"

GCMS_lists <- lapply(1:length(RCPs),function(a){
  GCMS<-as.character(list.dirs(paste0(grid_dir,"/",RCPs[[a]],"/",PERIODS[[1]]),
                               full.names = F,recursive = F)
  )
})
# RCP<-RCPs[[1]]
# GCM<-GCMS[[1]]
# PERIOD<-PERIODS[[1]]

source(paste0(src.dir,"/","_projecting.R"))
source(paste0(src.dir,"/","mclapply2.R"))


##########################################################
###RUNNING FOR GCM AND PERIODS 3 cores|3 cores| 2 core
###USING 18 CORES!
######################################################## 
#rm(sp_Dir)

if(!file.exists(paste0(sp_Dir_Original,"/","ensemble"))){
  #sp_Dir_Original

   
for(k in 1:length(RCPs)){
  GCMS <- GCMS_lists[[k]]
  cat("Projecting for: ",as.character(RCPs[[k]]),"\n")

  #lapply(1:length(GCMS),function(j){
  mclapply2(1:length(GCMS),function(j){
  #lapply(1:length(PERIODS),function(i){
    
  mclapply2(1:length(PERIODS),function(i){
    
  
    x<-projection_function(input_dir,RCP=RCPs[[k]],GCM=GCMS[[j]],PERIOD=PERIODS[[i]],var_cho,m2,out_dir,m2_rep,models_to_ensemble,m2_eval,mInfo,evaluation);gc()
    
    })
  })


};rm(k)

}else{
  cat("Already projected!","\n")
  }

##########################################################
###ENSEMBLING APPROACH PER RCP
######################################################## 
##########################################################
###RUNNING FOR GCM AND PERIODS 3 cores|3 cores| 2 core
###USING 18 CORES!
######################################################## 
if(!file.exists(paste0(sp_Dir_Original,"/","Graphics"))){
  
source(paste0(src.dir,"/","_ensemble_final.R"))

for(i in 1:length(RCPs)){
  
  cat("Projecting for: ",as.character(RCPs[[i]]),"\n")
  GCMS <- GCMS_lists[[i]]
  mclapply2(1:length(PERIODS),function(j){
    

x<-ensemble_RCP_function(mod_Dir,sp_name,sp_Dir_Original,RCP=RCPs[[i]],PERIOD=PERIODS[[j]],GCMS=GCMS)
  })
};rm(i)
}else{
cat("Already ensembled!","\n")
}
##########################################################
###NO MIGRATION SCENARIO FOR RCP
######################################################## 
if(!file.exists(paste0(sp_Dir_Original,"/","Graphics"))){
  
source(paste0(src.dir,"/","_No_migration_Scenario.R"))

  for(k in 1:length(RCPs)){
    
    cat("Projecting for: ",as.character(RCPs[[k]]),"\n")
    
    mclapply2(1:length(GCMS),function(j){
      
      mclapply2(1:length(PERIODS),function(i){
        
       x<-No_migration_Scenario_function(mod_Dir,sp_name,sp_Dir_Original,RCP=RCPs[[k]],PERIOD=PERIODS[[i]],GCM=GCMS[[j]],models_to_ensemble_MOD,current_Out_Dir)
          
      })
    }) 
  };

source(paste0(src.dir,"/","_No_migration_Scenario_RCP.R"))


for(k in 1:length(RCPs)){
  
  cat("Projecting for: ",as.character(RCPs[[k]]),"\n")
  
  mclapply2(1:length(PERIODS),function(i){
  
x<-No_migration_RCP_function(mod_Dir,sp_name,sp_Dir_Original,RCP=RCPs[[k]],PERIOD=PERIODS[[i]],models_to_ensemble_MOD,current_Out_Dir)
  })
};rm(i) 
}else{
  
  cat("Already ensembled!","\n")
}
##########################################################
###CALCULATING STEPWISE

source(paste0(src.dir,"/","_stepwise.R"))

x<-STEPWISE_STEP_FUNCTION(sp_Dir_Original,current_Out_Dir,GCMS,PERIODS,RCPs,grid_dir,spData_presence)
  
########################################################## 
###AFFECTED AREAS BY CLIMATE CHANGE
graphics_dir<-paste0(sp_Dir_Original,"/","Graphics");if(!file.exists(graphics_dir)){dir.create(graphics_dir)}
source(paste0(src.dir,"/","_affected_areas.R"))
x<-afffected_areas_function(graphics_dir,current_Out_Dir,ens_dir,RCPs,GCMS,PERIOD,grid_dir,var_cho,sp_Dir_Original)
  
source(paste0(src.dir,"/","_rcp_ensemble_affected_areas.R"))
x<-RCP_ENSEMBLE_AFFECTED_AREAS(sp_Dir_Original,RCPs,PERIODS,current_Out_Dir)

########################################################## 
###FINAL MAPS
sp_name2<-"Brachiaria brizantha"
countries<-c("Colombia", "Ecuador", "Peru","Venezuela","Brazil","Bolivia")

source(paste0(src.dir,"/","_maps.R"))

x<-maps_function(current_Out_Dir,RCPs,GCMS,PERIODS,sp_name2,countries)
  
####

source(paste0(src.dir,"/","_maps_affected_areas.R"))

x<-maps_function_af_areas(sp_Dir_Original,RCPs,GCMS,PERIODS,sp_name2,countries,current_Out_Dir) 
#TURN ON
# }else{
# cat("NO VALID MODELS, FINISHING THE SCRIPT!")
# }
