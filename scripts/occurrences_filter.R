###Joining occurrences scripts )GBIF + 1001genomes
#!USE CSV FILES 



require(maptools);data(wrld_simpl)
require(ff);require(sf);require(sp);require(rgdal);require(raster);require(shapefiles)
mainDir <- "//dapadfs/workspace_cluster_6/CWR/CWR_PROJECT_CC_BD/ccsosa/CLIMATE_CHANGE"
occDir <- paste0(mainDir,"/","occurrences")
inDir <- paste0(occDir,"/","genesys_raw")

occurrences <- as.data.frame(read.csv(paste0(occDir,"/","occurrence.txt"),sep="\t"))
occurrences <- occurrences[which(!is.na(occurrences$decimalLatitude)),]


occurrences <- as.data.frame(cbind(occurrences$gbifID,occurrences$decimalLongitude,occurrences$decimalLatitude))
occurrences$Source <- NA; occurrences$Source <- rep("GBIF",nrow(occurrences))
genomes_1000_csv <- as.data.frame(read.csv(paste0(occDir,"/","1001Genomes.csv"),sep="|"))
genomes_1000_csv <- as.data.frame(cbind(genomes_1000_csv$CS_number,genomes_1000_csv$longitude,genomes_1000_csv$latitude))
genomes_1000_csv$Source <- NA; genomes_1000_csv$Source <- rep("genomes_1001",nrow(genomes_1000_csv))


genesys <- as.data.frame(read.csv(paste0(occDir,"/","genesys.csv"),sep=","))
genesys <- genesys[which(!is.na(genesys$longitude)),]
genesys <- as.data.frame(cbind(genesys$id,genesys$longitude,genesys$latitude))
genesys$Source <- NA; genesys$Source <- rep("genesys",nrow(genesys))

final_file <- rbind(genomes_1000_csv, occurrences,genesys)
colnames(final_file) <- c("id","longitude","latitude","source")
# plot(occurrences$decimalLongitude,occurrences$decimalLatitude)
# plot(wrld_simpl,add=T)

plot(final_file$longitude,final_file$latitude,pch=16)#,xlim=c(-10,50),ylim=c(10,60))
plot(wrld_simpl,add=T)


write.csv(final_file,paste0(occDir,"/","occurrences_all.csv"),quote=F,row.names=F)


