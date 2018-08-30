inputDir <- "Z:/CLIMATE_CHANGE"
compressDir <- paste0(inputDir,"/","compress"); if(!file.exists(compressDir)){dir.create(compressDir)}
layersDir <- paste0(inputDir,"/","biolayers"); if(!file.exists(layersDir)){dir.create(layersDir)}

RCPs<-c("rcp2_6","rcp4_5","rcp6_0","rcp8_5")
PERIODS<-c("2050s","2080s")

BIOS_TO_USE <- list.files(path = compressDir,pattern="_bio_2_5min_r1i1p1")

lapply(1:length(RCPs),function(i){
  RCP_dir <- paste0(layersDir,"/",RCPs[[i]]); if(!file.exists(RCP_dir)){dir.create(RCP_dir)}
  RCP_FILES <- BIOS_TO_USE[grep(RCPs[[i]],BIOS_TO_USE)]
  
lapply(1:length(PERIODS),function(j){
  PER_dir <- paste0(RCP_dir,"/",PERIODS[[j]]); if(!file.exists(PER_dir)){dir.create(PER_dir)}
  RCP_FILES <- RCP_FILES[grep(PERIODS[[j]],RCP_FILES)]
  GCMS <-  sub(paste0("_",RCPs[[i]],"_",PERIODS[[j]],"_","bio_2_5min_r1i1p1_no_tile_grd.zip"),"",RCP_FILES)
    
lapply(1:length(GCMS),function(k){
  GCM_dir <- paste0(PER_dir,"/",GCMS[[k]]); if(!file.exists(GCM_dir)){dir.create(GCM_dir)}
  GCM_FILE <- paste0(compressDir,"/",GCMS[[k]],"_",RCPs[[i]],"_",PERIODS[[j]],"_bio_2_5min_r1i1p1_no_tile_grd.zip")
  unzip(GCM_FILE,exdir=GCM_dir) 
  
  cat(paste0("  ",k," of ",length(GCMS)," | ",RCPs[[i]],"_",PERIODS[[j]],".."),"\n")
     })
  cat(".....","\n")
  })
cat("       ","\n")
})
