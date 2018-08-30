require(raster);require(shapefiles);require(ggplot2);require(ggmap);require(reshape2)
maps_function_af_areas<-function(sp_Dir_Original,RCPs,GCMS,PERIODS,sp_name2,countries,current_Out_Dir){
 
  
  ##########################################################
  split_text <- unlist(strsplit(sp_name2, split=' '))
  cond <- c('subsp.', 'var.', 'f.')
  
  selected_text <- split_text[split_text %in% cond]
  
  
  cat("Savig file for ", as.character(sp_name2),"\n")
  
  
  if(length(selected_text)==0){
    
    NAME<-substitute(expr=italic(sp_name2),env=list(sp_name2=sp_name2))
  }else{
    a<-split_text[[1]]
    b<-split_text[[2]]
    c<-split_text[[4]]
    
    NAME<-substitute(expr=paste(italic(a)," ",italic(b)," ",selected_text," ",italic(c)),
                     env=list(a=a,b=b,selected_text=selected_text,c=c))
    
  }
  
  
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
  
  
  
  out_dir<-paste0(sp_Dir_Original,"/","ensemble")
  
  
  
  
input_dir_g<-paste0(sp_Dir_Original,"/","ensemble")
af_area<-stack(brick(paste0(input_dir_g,"/","affected_areas_all.img")))
names(af_area)<-ad

SCENARIOS_TO_MAP<-unique(c(ad2))



ad2_complete<-as.data.frame(c(ad2))
ad2_complete$ID<-NA;ad2_complete$ID<-1:nrow(ad2_complete);
colnames(ad2_complete)<-c("SCENARIO","ID")



lapply(1:length(SCENARIOS_TO_MAP),function(i){
  
  x<-ad2_complete$ID[which(ad2_complete$SCENARIO==SCENARIOS_TO_MAP[[i]])]
  x_names<-ad[x]
  xy<-stack(af_area[[x]])
 #
map.p <- as.data.frame(rasterToPoints(xy));gc()


dat.m_N_AF <- melt(map.p,id.vars=c("x","y"), measure.vars=colnames(map.p[,-c(1,2)]));gc()

colnames(dat.m_N_AF) <- c("Longitude", "Latitude","Scenario","MAP")
df<-dat.m_N_AF
usa <- map_data("world")
usa <- subset(usa, region %in% countries)

df<-df[!is.na(df$MAP),]
df<-df[df$MAP!=0,]



df$MAP[which(df$MAP==-1)]<-"Lost areas"
df$MAP[which(df$MAP==1)]<-"No affected areas"
df$MAP[which(df$MAP==2)]<-"New areas"

df$MAP<-factor(df$MAP,c("Lost areas",
                         "No affected areas",
                         "New areas"))

df$MAP<-factor(df$MAP)



map2<-ggplot(data=df,aes(y=Latitude, x=Longitude,fill=MAP)) +
  geom_polygon(data=usa,aes(x=long,y=lat,group=group),fill="gray70",na.rm = T, show.legend = F) +
  
  geom_raster(show.legend=F,interpolate=T,na.rm=T) +
  
  geom_polygon(data=usa,aes(x=long,y=lat,group=group),colour="gainsboro",fill=NA,na.rm = T, show.legend = F, size = 0.5) +
  
  facet_wrap(~Scenario,scales="free")

#map2<-map2+

#coord_fixed(1.3)+
#geom_path(color="white") +
#coord_equal() 

#map2<-map2+coord_fixed(xlim = c(extent(xy)[1],extent(xy)[2]),  ylim = c(extent(xy)[3],extent(xy)[4]), ratio = 1.3)

map2<-map2+
  ggtitle(NAME)+
  labs(x ="Longitude", y="Latitude")+
  #scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
  scale_fill_manual(values=c('#FC7277','#03B93B','#639CFB'))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
       legend.position="bottom",
        #legend.position="none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey40"),
        # panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.title=element_text(colour="black", size=16, face="bold"))

#(xy)
map2<-map2+coord_fixed(xlim = c(extent(xy)[1],extent(xy)[2]),  ylim = c(extent(xy)[3],extent(xy)[4]), ratio = 1.3)

# map2

# map3<-map2+facet_wrap(~Scenario,nc=4)
#+
name_to_save<-SCENARIOS_TO_MAP[[i]]
name_to_save<-(strsplit(name_to_save,split = " | "))
name_to_save<-paste0(name_to_save[[1]][1],"_",name_to_save[[1]][3])

ggsave(paste0(input_dir_g,"/","AFF_AREAS_",as.character(name_to_save),"_",Sys.Date(),".pdf"),map2,units="in",width=8,height=6,scale=2,dpi=600)


})
###############
#RCP_AFFECTED_AREAS
cat("RCP ensemble maps (AFFECTED AREAS)","\n")

cat("Calling thresholded current SDM","\n")
bl_prob_thr<-raster(paste0(current_Out_Dir,"/","MCAA_FINAL_THR.tif"))
bl_prob_thr_mask<-bl_prob_thr
bl_prob_thr_mask[which(bl_prob_thr_mask[]==0)]<-NA
names(bl_prob_thr_mask)<-"CURRENT"


RCP_AREAS_MAPS_AF<-lapply(1:length(RCPs),function(i){
  RCP<-RCPs[[i]]
  
  xy<-lapply(1:length(PERIODS),function(j){
    PERIOD<-PERIODS[[j]]
    
    RCP_out_dir<-paste0(out_dir,"/",RCP)
    PERIOD_out_dir<-paste0(RCP_out_dir,"/",PERIOD)
    
    x<-raster(paste0(PERIOD_out_dir,"/","Affected_areas.tif"))
    
    names(x)<-paste0(RCP,"|",PERIOD)
    return(x)
  })
  return(xy)
})

RCP_AREAS_MAPS_AF <- unlist(RCP_AREAS_MAPS_AF, recursive = FALSE)

RCP_AREAS_MAPS_AF<-stack(c(bl_prob_thr_mask,RCP_AREAS_MAPS_AF))

##
##
map.p_AF <- as.data.frame(rasterToPoints(RCP_AREAS_MAPS_AF));gc()


dat.map.p_AF <- melt(map.p_AF,id.vars=c("x","y"), measure.vars=colnames(map.p_AF[,-c(1,2)]));gc()

colnames(dat.map.p_AF ) <- c("Longitude", "Latitude","Scenario","MAP")
df<-dat.map.p_AF 
usa <- map_data("world")
usa <- subset(usa, region %in% countries)

df<-df[!is.na(df$MAP),]
df<-df[df$MAP!=0,]



df$MAP[which(df$MAP==-1)]<-"Lost areas"
df$MAP[which(df$MAP==1)]<-"No affected areas"
df$MAP[which(df$MAP==2)]<-"New areas"

df$MAP<-factor(df$MAP,c("Lost areas",
                        "No affected areas",
                        "New areas"))

df$MAP<-factor(df$MAP)



map2<-ggplot(data=df,aes(y=Latitude, x=Longitude,fill=MAP)) +
  geom_polygon(data=usa,aes(x=long,y=lat,group=group),fill="gray70",na.rm = T, show.legend = F) +
  
  geom_raster(show.legend=F,interpolate=T,na.rm=T) +
  
  geom_polygon(data=usa,aes(x=long,y=lat,group=group),colour="gainsboro",fill=NA,na.rm = T, show.legend = F, size = 0.5) +
  
  facet_wrap(~Scenario,scales="free")

#map2<-map2+

#coord_fixed(1.3)+
#geom_path(color="white") +
#coord_equal() 

#map2<-map2+coord_fixed(xlim = c(extent(xy)[1],extent(xy)[2]),  ylim = c(extent(xy)[3],extent(xy)[4]), ratio = 1.3)

map2<-map2+
  ggtitle(NAME)+
  labs(x ="Longitude", y="Latitude")+
  #scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
  scale_fill_manual(values=c('#FC7277','#03B93B','#639CFB'))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.position="bottom",
        #legend.position="none",
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey40"),
        # panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.title=element_text(colour="black", size=16, face="bold"))

#(xy)
map2<-map2+coord_fixed(xlim = c(extent(xy)[1],extent(xy)[2]),  ylim = c(extent(xy)[3],extent(xy)[4]), ratio = 1.3)

# map2

# map3<-map2+facet_wrap(~Scenario,nc=4)
#+





ggsave(paste0(input_dir_g,"/","AFF_AREAS_FINAL","_",Sys.Date(),".pdf"),map2,units="in",width=8,height=6,scale=2,dpi=600)



###
###

}
