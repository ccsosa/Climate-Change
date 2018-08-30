require(ff);require(sf);require(sp);require(rgdal);require(raster);require(shapefiles)
mainDir <- "//dapadfs/workspace_cluster_6/CWR/CWR_PROJECT_CC_BD/ccsosa/CLIMATE_CHANGE"
occDir <- paste0(mainDir,"/","occurrences")
inDir <- paste0(occDir,"/","genesys_raw")
core <- read.csv.ffdf(x=NULL,
                              file=paste0(inDir,"/","core.csv"),
                              encoding="UTF-8",
                              sep = ",",
                              VERBOSE = TRUE,
                              na.strings="",
                              first.rows = 20000,
                              next.rows = 10000,
                              # FUN= "read.csv",
                              quote="",
                              #quote="/"",
                              header=T,
                              colClasses=rep("factor",235)
)#,

####

geo <- read.csv.ffdf(x=NULL,
                      file=paste0(inDir,"/","geo.csv"),
                      encoding="UTF-8",
                      sep = ",",
                      VERBOSE = TRUE,
                      na.strings="",
                      first.rows = 20000,
                      next.rows = 10000,
                      # FUN= "read.csv",
                      quote="",
                      #quote="/"",
                      header=T,
                      colClasses=rep("factor",8)
)#,


coll <- read.csv.ffdf(x=NULL,
                     file=paste0(inDir,"/","coll.csv"),
                     encoding="UTF-8",
                     sep = ",",
                     VERBOSE = TRUE,
                     na.strings="",
                     first.rows = 20000,
                     next.rows = 10000,
                     # FUN= "read.csv",
                     quote="",
                     #quote="/"",
                     header=T,
                     colClasses=rep("factor",8)
)#,


join_table <- merge(core,coll,"X.genesysId.")
join_table2 <- merge(join_table,geo,"X.genesysId.")

final_table <- as.data.frame(cbind(as.character(join_table2$X.genesysId),
                     as.character(join_table2$X.fullTaxa.),
                     as.character(join_table2$X.orgCty.),
                     as.character(join_table2$X.longitude.),
                     as.character(join_table2$X.latitude.)
                     )
)
colnames(final_table) <- c("id","taxon","country","longitude","latitude")

#final_table$id <- as.numeric(final_table$id)
final_table$id <- as.character(final_table$id)
final_table$id <- sub("\"","",final_table$id)
final_table$id <- sub("\"","",final_table$id)
final_table$id <- as.numeric(final_table$id)

final_table$country <- as.character(final_table$country)
final_table$country <- sub("\"","",final_table$country)
final_table$country <- sub("\"","",final_table$country)

final_table$longitude <- as.character(final_table$longitude)
final_table$longitude <- sub("\"","",final_table$longitude)
final_table$longitude <- sub("\"","",final_table$longitude)
final_table$longitude <- as.numeric(final_table$longitude)

final_table$latitude <- as.character(final_table$latitude)
final_table$latitude <- sub("\"","",final_table$latitude)
final_table$latitude <- sub("\"","",final_table$latitude)
final_table$latitude <- as.numeric(final_table$latitude)

final_table$source <- NA;final_table$source <- rep("genesys",nrow(final_table))


for(i in 1:nrow(final_table)){
  if(final_table$longitude[[i]] == final_table$latitude[[i]]){
    cat("bad coords: ",i,"\n")
    final_table$longitude[[i]] <- NA; final_table$latitude[[i]] <- NA
  } else if(final_table$longitude[[i]] ==0 & final_table$latitude[[i]] ==0){
    cat("0 coords: ",i,"\n")
        final_table$longitude[[i]] <- NA; final_table$latitude[[i]] <- NA
  }else if(final_table$longitude[[i]] ==0 & final_table$latitude[[i]] !=0){
    cat(" longitude wrong: ",i,"\n")
    final_table$longitude[[i]] <- NA; final_table$latitude[[i]] <- NA
  }else if(final_table$longitude[[i]] !=0 & final_table$latitude[[i]] ==0){
    cat(" latitude wrong: ",i,"\n")
    final_table$longitude[[i]] <- NA; final_table$latitude[[i]] <- NA
  }
}
final_table <- final_table[which(!is.na(final_table$longitude) & !is.na(final_table$latitude) ),]
final_table2 <- final_table
#####
plot(final_table$longitude,final_table$latitude)
#countries <- sf::read_sf(dsn = paste0(mainDir,"/","geodata/shapefile"), layer = "gadm28ISO")
countries <- shapefile(paste0(mainDir,"/","geodata/shapefile","/","gadm28ISO.shp"))
#####
  sp::coordinates(final_table2) <- ~longitude+latitude
crs(final_table2) <- crs(countries)
  #::st_crs(countries)$proj4string
#final_table2 <- sf::st_as_sf(final_table2)
#ov <- st_intersects(final_table2,countries)

#ov2 <- st_intersection(final_table2,countries)
ovr <- over(final_table2, countries)
cntr <- ovr$ISO
i <- which(!is.na(cntr))

final_table3 <- as.data.frame(final_table2)
final_table3$ISO <- NA
final_table3$ISO <-ovr$ISO
final_table3 <- final_table3[!is.na(final_table3$ISO),]
final_table3$MATCH <- NA

for(i in 1:nrow(final_table3)){
  if(final_table3$country[[i]]==final_table3$ISO[[i]]){
    final_table3$MATCH[[i]] <- "MATCH"
  } else  {
    final_table3$MATCH[[i]] <- "CHECK"
  }
}


final_table3 <- final_table3[which(final_table3$MATCH=="MATCH"),]


write.csv(final_table3,paste0(occDir,"/","genesys.csv"),quote=F,row.names=F)


