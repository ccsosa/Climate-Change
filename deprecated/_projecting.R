require(raster)

cat("##############################","\n")
cat("                              ","\n")
cat("LOADING PROJECTING SCRIPT!    ","\n")
cat("                              ","\n")
cat("##############################","\n")
cat("                              ","\n")


  
  projection_function<-function(input_dir,RCP,GCM,PERIOD,var_cho,m2,out_dir,m2_rep,models_to_ensemble,m2_eval,mInfo,evaluation){

   
    ##########################################################
    ###FUTURE GRID DIR    
    input_dir<-grid_dir
    ##########################################################
    ###PROJECTING DIR     
    out_dir<-sp_Dir_Original
    ##########################################################
    ###CREATING PROJECTION FOLDERS 
    ######################################################## 
    proj_dir<-paste0(sp_Dir_Original,"/","projection");if(!file.exists(proj_dir)){dir.create(proj_dir)}
    ##########################################################
    ###CREATING PROJECTION FOLDERS RCP FOLDER
    RCP_proj_dir<-paste0(proj_dir,"/",RCP);if(!file.exists(RCP_proj_dir)){dir.create(RCP_proj_dir)}
    ##########################################################
    ###CREATING PROJECTION FOLDERS GCM FOLDER
    GCM_proj_dir<-paste0(RCP_proj_dir,"/",GCM);if(!file.exists(GCM_proj_dir)){dir.create(GCM_proj_dir)}
    ##########################################################
    ###CREATING PROJECTION FOLDERS PERIOD FOLDER
    PERIOD_proj_dir<-paste0(GCM_proj_dir,"/",PERIOD);if(!file.exists(PERIOD_proj_dir)){dir.create(PERIOD_proj_dir)}
    
    
    ##########################################################
    ###FUTURE GRID DIR    
    ##########################################################
    
    
    ##########################################################
    # ##CALLING LAYERS  
    RCP_dir<-paste0(input_dir,"/",RCP)
    GCM_dir<-paste0(RCP_dir,"/",GCM)
    PERIOD_dir<-paste0(GCM_dir,"/",PERIOD)
    
    ad<-PERIOD_dir;ad<-sub(paste0(input_dir,"/"),"",ad);ad<-sub("/"," | ",ad);ad<-sub("/"," | ",ad);
    
    
    
    
    verFile <- paste(PERIOD_proj_dir,"/", "MODEL",".run", sep="")
    
    if (!file.exists(verFile)) {
      
      
    cat("                              ","\n")
    cat("USING:                        ","\n")
    cat(" RCP  |     GCM    |  PERIOD  ","\n")
    cat(ad,"\n")
    cat("                              ","\n")
    
    
           
###########################################################  
target_dir<-paste0(PERIOD_dir,"/","biovars")
###########################################################      
    
prj_clim_layer<-lapply(paste0(target_dir,"/","/",paste0(var_cho,".tif")),raster)
prj_clim_layer<-stack(prj_clim_layer);gc()
cat("                              ","\n")
cat("SAVING MULTILAYER FILES FOR:  ","\n")
cat(" RCP  |     GCM    |  PERIOD  ","\n")
cat(ad,"\n")
cat("                              ","\n")

##########################################################
###PREDICTING REPLICATES
if(!file.exists(paste0(PERIOD_proj_dir,"/",'proj_all.img'))){
  cat("Projecting raster file already saved, using it","\n")
p2m <- predict(m2,newdata=prj_clim_layer,filename=paste0(PERIOD_proj_dir,"/",'proj.img'),nc=3,mean=T,overwrite=F);gc()
p2m_all <- predict(m2,newdata=prj_clim_layer,filename=paste0(PERIOD_proj_dir,"/",'proj_all.img'),nc=3,mean=F,overwrite=F);gc()

}else{

cat("Projecting raster file already saved, using it","\n")

p2m_all<-stack(brick(paste0(PERIOD_proj_dir,"/",'proj_all.img')))

}

cat("                              ","\n")
cat("ENSEMBLING FILES FOR:         ","\n")
cat(" RCP  |     GCM    |  PERIOD  ","\n")
cat(ad,"\n")
cat("                              ","\n")

##########################################################
###ENSEMBLING

source(paste0(src.dir,"/","_evaluation_ens.R"))
sp_Dir<-PERIOD_proj_dir
x<-eval_to_ens(models_to_ensemble,m2_eval,sp_Dir,mInfo,m2_rep,p2m_all);gc()


#Run verification file
verFile <- paste(PERIOD_proj_dir,"/", "MODEL",".run", sep="")
opnFile <- file(verFile, open="w")
cat("Modelled on", date(), file=opnFile)
close.connection(opnFile)

    
cat("                              ","\n")
cat(" RCP  |     GCM    |  PERIOD  ","\n")
cat(ad,"\n")
cat("                              ","\n")
cat("DONE!                         ","\n")
cat("                              ","\n")

        
}else{
  cat("The species was already projected for: ","\n")
  cat(" RCP  |     GCM    |  PERIOD  ","\n")
  cat(ad,"\n")
  cat("                                       ","\n")
  
     }
}